# knosh

⚠️ under heavy development ⚠️

Keeping the spirit of the lisp machine alive in a frankensteinian combination sh and lisp.

This is lisp-ey shell replacement, with a few ~~hacks~~features bolted on to make it more usable as a daily driver. The goal is to have about the same level of productivity as bash, but at the same time allow you to scale up the lines of code without your script turning into a giant bag of string-escaping mush.

Underlying knosh is [ketos](https://github.com/murarth/ketos), a minimalistic lisp designed for ease of embeddability.

The name in shorthand for the [knolling](https://en.wikipedia.org/wiki/Tom_Sachs_\(artist\)#Knolling) shell. Or maybe the ketos non-shell.

## Installation

Install rust, then run:

```
git clone git@github.com:ysimonson/knosh.git
cd knosh
cargo install --path .
```

## Language

Knosh uses ketos. See its [docs](https://github.com/murarth/ketos/tree/master/docs) for more information on the underlying language.

## Shortcuts

### Auto-expansion

In order to save keystrokes, knosh walks the AST at evaluation time and makes a number of changes:

1) Input expressions are automatically wrapped in parens if they don't have them already, so `cd "foo"` becomes `(cd "foo")`.
2) If the first argument in a list does not refer to a known function, it's automatically converted into a call to run a process, so `ls` becomes `(spawn "ls")`.
3) If any other argument in the list does not refer to a known name, it's automatically converted into a string, which allows it to act as an argument to a proc. So `ls -l` becomes `(spawn "ls" "-l")`.

### Auto-completions

Knosh uses the excellent [linefeed](https://github.com/murarth/linefeed) crate (by the same author as ketos!) so the repl offers all the ergonomics you've come to expect out of bash - autocomplete, reverse-i-search, history, etc.

Tab completions work on:
* Functions
* Variables
* Paths
* Proc names
* Proc args

## Builtins

Beyond ketos' [standard operators](https://github.com/murarth/ketos/blob/master/docs/operators.md) and [system functions](https://github.com/murarth/ketos/blob/master/docs/functions.md), knosh offers builtins to support all the goodies you'd typically expect out of a shell.

### Procs

Procs are child processes in knosh. You kickstart procs like so:

```lisp
(spawn '("ls" "-l"))
```

Procs can also be piped like so:

```lisp
(| ((ls -l) (grep foo)))
```

Spawn functions:
* `(spawn name:string arg1:stringifiable ... argn:stringifiable [stdio]) -> proc`: Spawns a process with the given name and arguments.
* `(spawn-with-stdio stdin:inputtable stdout:outputtable stderr:output name:string arg1:stringifiable ... argn:stringifiable)`: Spawns a process with the given name, arguments, and stdio. Valid inputtables are:
  * Another proc: it will take that proc's stdout.
  * A proc's stdout (fetched with `(stdout)`, see below.)
  * A proc's stderr (fetched with `(stderr)`, see below.)
  * `stdio/inherit`: Inherits the shell's stdin.
  * `stdio/piped`: Pipes the stdin.
  * `stdio/null`: The equivalent to `/dev/null`.
Valid outputtables are:
  * Another proc: it will take that proc's stdin.
  * A proc's stdin (fetched with `(stdin)`, see below.)
  * `stdio/inherit`: Inherits the shell's stdout or stderr.
  * `stdio/piped`: Pipes the stdout or stderr.
  * `stdio/null`: The equivalent to `/dev/null`.
* `(exec name:string arg1:stringifiable ... argn:stringifiable)`: Execs a process (i.e. the shell process is replaced) with the given name and arguments.

Proc management functions:
* `(wait p:proc) -> exitstatus`: Waits for a proc to finish, returning its exit status.
* `(poll p:proc) -> exitstatus|()`: Polls for the proc status. If the proc has finished, the exit status is returned. Otherwise it returns the unit value.
* `(exit/success? s:exitstatus) -> bool`: Returns whether the proc exit status is considered successful.
* `(exit/code s:exitstatus) -> integer|()`: If the proc had an exit code, this returns that code. Otherwise it returns the unit value.
* `(exit/signal s:exitstatus) -> integer|()`: If the proc was terminated by a signal, this returns that signal. Otherwise it returns the unit value.
* `(pid p:proc) -> integer`: Returns the proc's pid.

Stdio functions:
* `(| p1:proc ... pn:proc)`: Pipes procs's stdouts to stdins.
* `(stdin p:proc)`: Gets the stdin of `p`.
* `(stdout p:proc)`: Gets the stdout of `p`.
* `(stderr p:proc)`: Gets the stderr of `p`.
* `(stdin/write i:stdin bytes:bytes)`: Writes `bytes` to `i`.
* `(stdin/fd i:stdin) -> integer`: Returns `i`'s underlying fd.
* `(stdout/read o:stdout limit:integer) -> bytes`: Reads up to `limit` bytes from `o`. If `limit` is 0, everything until an EOF is returned.
* `(stdout/read-line o:stdout) -> string`: Reads up to the next newline from `o`, returning the decoded string.
* `(stdout/fd o:stdout) -> integer`: Returns `o`'s underlying fd.
* `(stderr/read e:stderr limit:integer) -> bytes`: Reads up to `limit` bytes from `e`. If `limit` is 0, everything until an EOF is returned.
* `(stderr/read-line e:stderr) -> string`: Reads up to the next newline from `e`, returning the decoded string.
* `(stderr/fd e:stderr) -> integer`: Returns `e`'s underlying fd.

### Subinterps

You can spawn subinterps:

```
(fork (lambda () (println "hello from a subinterp")))
```

This forks the current shell and executes the given function. The calling shell is given back a subinterp handle.

Functions:
* `(fork callback:lambda) -> subinterp`: Forks the process and executes the given function in the child shell. The parent shell is given back a subinterp handle.
* `(wait i:subinterp)`: Waits for a subinterp to finish, returning its exit status.

### Environment variables

Functions:
* `(set-env name:string value:string)`: Sets an environment variable.
* `(env name:string) -> string`: Gets an environment variable.
* `(del-env name:string)`: Deletes an environment variable.

### Paths

Functions:
* `(d [dir:string]) -> string`: If an argument is passed, this changes the current working directory. Afterwards, it returns the current working directory as an absolute path.
* `(cd dir:string) -> string`: Changes the current working directory.
* `(pwd) -> string`: Returns the current working directory as an absolute path.

### Signals

Functions:
* `(trap s:signal callback:lambda) -> integer`: Adds a callback to execute on a given signal. Returns a trap key.
* `(untrap s:signal i:integer)`: Removes a callback as identified by a signal and trap key.

Values:
* `signal/abrt`: SIGABRT
* `signal/alrm`: SIGALRM
* `signal/bus`: SIGBUS
* `signal/chld`: SIGCHLD
* `signal/cont`: SIGCONT
* `signal/hup`: SIGHUP
* `signal/int`: SIGINT
* `signal/io`: SIGIO
* `signal/pipe`: SIGPIPE
* `signal/prof`: SIGPROF
* `signal/quit`: SIGQUIT
* `signal/sys`: SIGSYS
* `signal/term`: SIGTERM
* `signal/trap`: SIGTRAP
* `signal/usr1`: SIGUSR1
* `signal/usr2`: SIGUSR2
* `signal/winch`: SIGWINCH

See [here](http://man7.org/linux/man-pages/man7/signal.7.html) for details on what the signals mean.

### Shell management

Functions:
* `(exit code:integer)`: exits the shell with the given code
* `(pid) -> integer`: Returns the shell's pid.
* `(stdin/read-line)`: Reads from the shell's stdin until a newline, returning a string.
* `(stdin/fd) -> integer`: Returns the shell's stdin fd.
* `(stdout/fd) -> integer`: Returns the shell's stdout fd.
* `(stderr/fd) -> integer`: Returns the shell's stderr fd.
