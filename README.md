# lfarm

lfarm is a Common Lisp library for distributing work across machines
using the [lparallel] (http://lparallel.org) API.

### Download

Assuming that you have [Quicklisp](http://www.quicklisp.org/beta/) installed,

    $ cd ~/quicklisp/local-projects
    $ git clone git://github.com/lmj/lfarm.git

lfarm is known to run on Allegro, Clozure, LispWorks, and SBCL.

### Kernel

In lparallel a _kernel_ was defined as abstract entity that schedules
and executes tasks. lparallel implements it with a thread pool, while
in lfarm it is implemented with a set of servers that execute tasks.

    ;; Create two servers bound to ports 11111 and 22222.
    (ql:quickload :lfarm-server)
    (lfarm-server:start-server "127.0.0.1" 11111 :background t)
    (lfarm-server:start-server "127.0.0.1" 22222 :background t)

    ;; Connect to the servers. `lfarm' is a package nickname for `lfarm-client'.
    (ql:quickload :lfarm-client)
    (setf lfarm:*kernel* (lfarm:make-kernel '(("127.0.0.1" 11111)
                                              ("127.0.0.1" 22222))))

    ;; Use the lparallel API.
    (let ((channel (lfarm:make-channel)))
      (lfarm:submit-task channel #'+ 3 4)
      (lfarm:receive-result channel))
    ;; => 7

Although the servers in this example are local, lfarm servers may run
in separate Lisp instances on remote machines.

### Tasks

There are some restrictions on a task slated for remote execution. A
task must be

1. a lambda form, or
2. a function that exists on the remote servers, or
3. a function defined with `deftask`.

`deftask` is just like `defun` except the function definition is
recorded. (A Lisp implementation may record a function definition, but
is not required to do so.)

    (defpackage :example (:use :cl :lfarm))
    (in-package :example)

    (deftask add (x y)
      (+ x y))

    (let ((channel (make-channel)))
      (submit-task channel #'add 3 4)
      (receive-result channel))
    ;; => 7

`submit-task` notices that `add` was defined with `deftask` and
converts it to a named lambda before submitting it to a server.

`deftask*` is a variant of `deftask` which records the function body
without defining the function.

To define `add` remotely use `broadcast-task`, which executes a given
task on all servers.

    (broadcast-task (lambda () (defun add (x y) (+ x y))))

Or more likely `add` would be part of a system that is loaded on all
servers.

    (broadcast-task #'ql:quickload :my-stuff)

Limited support for closures is available on SBCL, CCL, LispWorks, and
Allegro. Lexical variables and symbol macrolets are captured, but
`flet` functions are not.

Tasks are not macroexpanded in order to ensure portability across
clients and servers.

### API

The `lfarm-client` system defines the `lfarm-client` package which has
the nickname `lfarm`. It exports the [lparallel kernel
API](http://lparallel.org/api/kernel) with the following differences.

* tasks have the aforementioned restrictions placed upon them
* the addition of `deftask` and its non-locally-defining cousin `deftask*`
* `make-kernel` expects addresses, and lacks the `:context` and
  `:bindings` arguments
* `task-handler-bind` does not exist
* `*debug-tasks-p*` and `*kernel-spin-count*` exist but have no effect
* `submit-task` is a macro that wraps `submit-task*` (explained below)
* the addition of `broadcast-task` which similarly wraps `broadcast-task*`
* `task-execution-error` is signaled when a task fails on a remote
server, instead of the actual error (which may not have local meaning)

[Promises](http://lparallel.org/api/promises/) and a limited number of
[cognates](http://lparallel.org/api/cognates/) are also available,
found in the packages `lfarm-client.promise` and
`lfarm-client.cognate` respectively and also exported by
`lfarm-client`.

The systems `lfarm-server` and `lfarm-admin` provide the following functions.

* `lfarm-server:start-server host port &key background name` -- Start a
server instance listening at `host`:`port`. If `background` is true
then spawn the server in a separate thread named `name`.

* `lfarm-admin:ping host port &key timeout` -- Send a ping to the lfarm
server at `host`:`port`. Keep trying to make contact for `timeout`
seconds, or if `timeout` is nil then try forever. Default is 3
seconds. Returns true if successful and nil otherwise.

* `lfarm-admin:end-server host port` -- End the server at `host`:`port`.
This only stops new connections from being made. Connections in
progress are unaffected.

## Security

The purpose of an lfarm server is to execute arbitrary code, so it is
highly advised to enable some form of security. lfarm directly
supports Kerberos (or Active Directory) authentication. Alternatively,
SSH tunnels may be used.

### Security with SSH tunneling

    ;; On the remote machine
    (ql:quickload :lfarm-server)
    (lfarm-server:start-server "127.0.0.1" 33333)

To create a tunnel,

    # On the local machine
    $ ssh -f -L 33333:127.0.0.1:33333 <remote-address> -N

The remote server should now be accessible locally.

    ;; On the local machine
    (ql:quickload :lfarm-admin)
    (lfarm-admin:ping "127.0.0.1" 33333) ;=> T

Of course there is still local security to consider, as local users on
both ends have access to the server. If this is a concern then a
packet filtering tool such as iptables may be used.

### Security with Kerberos/GSSAPI

The `lfarm-gss` system provides support for GSSAPI authentication. The
`:auth` argument to `lfarm-server:start-server` and
`lfarm-client:make-kernel` accepts an instance of
`lfarm-gss:gss-auth-server` and `lfarm-gss:gss-auth-client`
respectively.

When creating a server, the class `lfarm-gss:gss-auth-server` accepts
the initialization keyword `:service-name`. This value is indicats
which service type should be used when requesting a ticket for the
remote service. The default is `lfarm`. In other words, if an attempt
is done to connect to the server at `server.example.com`, the service
principal will be `lfarm/server.example.com`.

When creating a kernel (client), the class `lfarm-gss:gss-auth-client`
accepts the initialization keyword `:allowed-users` which specifies a
list of all users that are allowed to connect to the server. Each
element should be a string representing the principal name (including
realm) of the user that is allowed to connect. For example:
`user@EXAMPLE.COM`.

If a more complex authorization mechanism is needed which is not
covered by the simple user list as described above, you can subclass
the `gss-auth-server` class and then implement the method
`lfarm-gss:name-accepted` on your new class. This generic function
takes two arguments, the authentication object and the name to be
verified, and should return non-NIL if the user is allowed to connect.
Note that the name is an instance of `cl-gss:name`, and you need to
call the function `cl-gss:name-to-string` on it to extract the actual
name.

The server needs to have access to the service principal in a keytab
file. How to create the keytab file depends on your Kerberos server
implementation:

* For MIT Kerberos:
  http://web.mit.edu/kerberos/krb5-1.5/krb5-1.5.4/doc/krb5-admin/Adding-Principals-to-Keytabs.html

* For Heimdal:
  http://www.h5l.org/manual/HEAD/info/heimdal/keytabs.html (don't
  forget to add the `-k` flag to specify the file to which the key
  should be written)

* For Active Directory:
  http://technet.microsoft.com/en-us/library/bb742433.aspx

Once you have the keytab file, make sure that the environment variable
`KRB5_KTNAME` is set to the path of the keytab file and that it is
readable by the lfarm server instance. If not, the server will not be
able to authenticate itself against the client which will prevent it
from connecting.

## Details

That covers perhaps all you need to know about lfarm. Those who are
curious may read on (or not).

### Serialization

Serialization is done with
[cl-store](http://common-lisp.net/project/cl-store/). It uses a
portable serialization format, allowing lfarm clients and servers to
run on different Lisp implementations.

### Packages

A symbol is deserialized on the remote server with its home package
intact. If the server encounters a symbol whose package does not
exist, an empty version of the package is automatically generated.

### Connection errors

The lfarm client is obstinate with regards to connections: if there is
a connection error then it tries to reconnect, and will continue
trying. We may therefore restart servers while using the same kernel
instance, or call `make-kernel` before any servers exist (the call
will block until they do).

Note it is possible for a task to be executed twice (or more). If a
connection error occurs in the time interval after a task has been
submitted and before its result has been received, the client will
attempt to submit the task again.

### submit-task

In lparallel `submit-task` is a function, but in lfarm it is a macro
that provides syntactic sugar for the function `submit-task*`.

    (submit-task channel #'+ 3 4)
    ;; =macroexpand=> (SUBMIT-TASK* CHANNEL '+ 3 4)

    (submit-task channel (lambda (x) (1+ x)) 3)
    ;; =macroexpand=> (SUBMIT-TASK* CHANNEL '(LAMBDA (X) (1+ X)) 3)

`submit-task` may alter the task argument before giving it to
`submit-task*`, which expects a symbol or a lambda form. Sharp-quote
is replaced with quote, and a lambda form gets quoted. This provides a
semblance with `lparallel:submit-task` and relieves us from having to
write `'(lambda ...)` and `'f` in place of `(lambda ...)` and `#'f`.

### Logging

Verbose logging is enabled by binding `lfarm-common:*log-level*` to
`:info` (default is `:error`). The log stream is
`lfarm-common:*log-stream*` (default is `*debug-io*`).

### Tests

The lfarm test suite assumes a working ssh executable is present and
that passwordless authorization has been set up for "ssh localhost".
To run it load the `lfarm-test` system and call `lfarm-test:execute`,
which may be given some configuration options. Unrecognized Lisp
implementations will require configuration (namely, specifying the
lisp executable and the command-line switch to eval a form). Tests
also assume that Quicklisp has been installed (but not necessarily
loaded), although configuration may remove this assumption.

### Implementation

The client has an internal lparallel kernel in which each worker
thread manages a connection to an assigned remote server, one worker
per server. When a worker connects to a server, the server enters a
task execution loop wherein a form is deserialized, maybe compiled,
and funcalled; repeat. A server may serve multiple clients.

Though an async backend is possible, this threaded implementation was
chosen because it was easy and portable.

Opportunities for optimization in the realm of remote task queues and
remote task stealing have been callously ignored. Task queues are
local.

### Author

James M. Lawrence <llmjjmll@gmail.com>

Kerberos support by Elias Martenson <lokedhs@gmail.com>
