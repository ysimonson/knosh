# knosh

⚠️ under heavy development ⚠️

Keeping the spirit of the lisp machine alive in a frankensteinian combination sh and lisp.

This is lisp-ey shell replacement, with a few ~~hacks~~features bolted on to make it more usable as a daily driver. The goal is to have about the same level of productivity as bash, but at the same time allow you to scale up the lines of code without your script turning into a giant bag of string-escaping mush.

Underlying knosh is [ketos](https://github.com/murarth/ketos), a minimalistic lisp designed for easy embedding. Knosh is not meant to be a backwards-compatible replacement to existing shell languages, though most of the commands you're used to running in bash work the same.

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
2) If the first argument in a list does not refer to a known function, it's automatically converted into a call to run a process, so `ls` becomes `(proc "ls")`.
3) If any other argument in the list does not refer to a known name, it's automatically converted into a string, which allows it to act as an argument to a proc. So `ls -l` becomes `(proc "ls" "-l")`.

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
(proc "ls" "-l")
```

This actually returns a _promise_ to a child process, which can then be resolved a number of ways:

1) If a proc is evaluated bare by the repl, it is executed inheriting the shell's stdio. So if you ran `(proc "ls" "-l")` directly in the repl, it would print the results of running `ls -l`.
2) You can pass it to `spawn`, which allows you to control the process' stdio and interact with it; e.g. `(spawn (proc "ls" "-l") (stdio/null stdio/null stdio/null))` would run `ls -l` but suppress the output.
3) You can pass it to `exec`, which will replace the shell with the specified proc; e.g. `(exec (proc "ls" "-l"))` will exec `ls -l`.

Functions:
* `(proc name:string, arg1:stringifiable, ..., argn:stringifiable) -> ProcPromise`: Creates a promise to execute a process with the given `name` and arguments; e.g. `(proc "ls" "-l")` returns a promise to execute `ls -l`.
* `(spawn p:ProcPromise [stdio]) -> Proc`: Executes a proc promise with the given stdio, returning a handle to the proc. `stdio` is zero to three arguments:
  * If it's zero arguments, the process inherits the shell's stdio
  * If it's one argument, it specifies the process' stdout, and the process inherits the shell's stdin and stderr.
  * If it's two arguments, it specifies the process' stdout and stderr, and the process inherits the shell's stdin.
  * If it's three arguments, it specifies the process' stdin, stdout, and stderr in that order.
* `(exec p:ProcPromise)`: Execs a promise, replacing the shell's process with the specified one.
* `(wait p:Proc) -> ExitStatus`: Waits for a proc to finish, returning its exit status.
* `(poll p:Proc) -> ExitStatus|()`: Polls for the proc status. If the proc has finished, the exit status is returned. Otherwise it returns the unit value.
* `(exit/success? s:ExitStatus) -> bool`: Returns whether the proc exit status is considered successful.
* `(exit/code s:ExitStatus) -> integer|()`: If the proc had an exit code, this returns that code. Otherwise it returns the unit value.
* `(exit/signal s:ExitStatus) -> integer|()`: If the proc was terminated by a signal, this returns that signal. Otherwise it returns the unit value.
* `(pid p:Proc) -> integer`: Returns the proc's pid.
* `(write p:Proc, bytes:bytestring)`: Writes the bytes to the proc's stdin. Note this only works if the proc's stdin is piped.
* `(stdin/fd p:Proc) -> integer`: Returns the proc's stdin fd.
* `(stdout/fd p:Proc) -> integer`: Returns the proc's stdout fd.
* `(stderr/fd p:Proc) -> integer`: Returns the proc's stderr fd.

Values:
* `stdio/inherit`: A value that specifies that a process should inherit the stdio from the shell.
* `stdio/piped`: A value that specifies that a process should pipe the stdio.
* `stdio/null`: A value that specifies that a process should suppress the stdio.

### Pipes

Procs can also be piped like so:

```lisp
(| (ls -l) (grep foo))
```

Similar to procs, this returns a _promise_ to a pipe, and can be resolved in two ways:

1) If a pipe is evaluated bare by the repl, it is executed inheriting the shell's stdio. So if you ran `(| (ls -l) (grep foo))`, it'd be the equivalent of running `ls -l | grep foo` in bash.
2) You can pass it to `spawn`, which allows you to control the pipe's stdio and interact with it; e.g. `(spawn (| (ls -l) (grep foo)) (stdio/null stdio/null stdio/null))` would run `ls -l` but suppress the output.

Functions:
* `(| p1:ProcPromise, ..., pn:ProcPromise) -> PipePromise`: Creates a promise to execute a pipe with the given proc promises.
* `(spawn p:PipePromise [stdio]) -> Pipe`: Executes a proc promise with the given stdio, returning a handle to the pipe. `stdio` is the same as those documented above for procs.
* `(wait p:Pipe)`: Waits for all procs in the pipe to finish. After all of the procs are finished, the first error that was encountered is thrown.

### Subinterps

You can spawn subinterps:

```
(fork (lambda () (println "hello from a subinterp")))
```

This forks the current shell and executes the given function. The calling shell is given back a subinterp handle.

Functions:
* `(fork callback:lambda) -> SubInterp`: Forks the process and executes the given function in the child shell. The parent shell is given back a subinterp handle.
* `(wait i:SubInterp)`: Waits for a subinterp to finish, returning its exit status.

### Environment variables

Functions:
* `(set-env name:string value:string)`: Sets an environment variable.
* `(env name:string) -> string`: Gets an environment variable.
* `(del-env name:string)`: Deletes an environment variable.

### Paths

Functions:
* `(cd dir:string) -> string`: Changes the current working directory and returns the new absolute path.
* `(pwd) -> string`: Returns the current working directory as an absolute path.

### Traps

**Note:** At the moment, traps only work in the repl, and cannot be used from executing scripts.

Functions:
* `(trap s:Signal callback:lambda) -> integer`: Adds a callback to execute on a given signal. Returns a trap key.
* `(untrap s:Signal i:integer)`: Removes a callback as identified by a signal and trap key.

Values:
* `signal/continue`: A value representing `SIGCONT`.
* `signal/interrupt`: A value representing `SIGINT`.
* `signal/quit`: A value representing `SIGQUIT`.
* `signal/resize`: A value representing `SIGWINCH` (window resize.)
* `signal/suspend`: A value representing `SIGTSTP`.

### Shell management

Functions:
* `(exit code:integer)`: exits the shell with the given code
* `(pid) -> integer`: Returns the shell's pid.
* `(stdin/fd) -> integer`: Returns the shell's stdin fd.
* `(stdout/fd) -> integer`: Returns the shell's stdout fd.
* `(stderr/fd) -> integer`: Returns the shell's stderr fd.
