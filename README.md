# Lake

[![Circle CI](https://circleci.com/gh/takagi/lake.svg?style=shield)](https://circleci.com/gh/takagi/lake)
[![Coverage Status](https://coveralls.io/repos/takagi/lake/badge.svg?branch=master&service=github)](https://coveralls.io/github/takagi/lake?branch=master)

Lake is a GNU make or Ruby's [rake](https://github.com/ruby/rake) like build utility in Common Lisp. Instead of Makefile or Rakefile, it uses Lakefile where tasks executed are defined.

Make is, originally, a program to build executable files to compile multiple source files and resolve their dependency, however its use case may not be limited as a build utility but shell command management is also good. Lake's main use case would be the latter.

## Usage

In lake, you use `Lakefile` instead of `Makefile` or `Rakefile`.

    (use-package '(:lake :cl-syntax))
    (use-syntax :interpol)

    ;; Tasks that build an executable with dependency.
    (defparameter cc (or (ros:getenv "CC") "gcc"))

    (file "hello" ("hello.o" "message.o")
      (sh #?"${cc} -o hello hello.o message.o"))
    
    (file "hello.o" ("hello.c")
      (sh #?"${cc} -c hello.c"))
    
    (file "message.o" ("message.c")
      (sh #?"${cc} -c message.c"))
    
    (task "clean" ()
      (sh "rm -rf hello hello.o message.o"))

From REPL, you can call `hello` task with the following form. Ensure that the REPL process' current directory is same as the `Lakefile`.

    (lake:lake :target "hello")

Or you can also call it from command line, which would be the main use case of lake.

    $ lake hello

As a tip, you can generate an empty Lakefile with package related boilerplates in the current directory as:

    $ lake-tools init

Further detail, please see [Lakefile](#Lakefile) section and [API](#API) section.

## Install

You can install lake via Quicklisp,

    (ql:quickload :lake)

or you can also install it using Roswell including `lake` command. Ensure that `PATH` environment variable contains `~/.roswell/bin`.

    $ ros install lake
    $ which lake
    /path/to/home/.roswell/bin/lake

## Lakefile

Lake provides the following forms to define tasks and namespaces in `Lakefile`:
- **Task** fundamental concept processing a sequence of shell commands
- **File Task** a task resolving file dependency with up-to-date check
- **Directory Task** a task that ensures a directory exists.
- **Namespace** grouping up multiple tasks to magage them.

### Task

    TASK task-name dependency-list [description] form*

`task` represents a sequence of operations to accomplish some task. `task-name` is a string that specifies the target task by its name. `dependency-list` is a list of task names on which the target task depends. The dependency task names are given in both relative and absolute manner, which begins with a colon `:`. `description` is a doc string. `form`s can be any Common Lisp forms.

    $ cat Lakefile
    ...
    (namespace "hello"

      (task "foo" ("bar")               ; dependency task in relative manner
        (echo "hello.foo"))

      (task "bar" (":hello:baz")        ; dependency task in absolute manner
        (echo "hello.bar"))

      (task "baz" ()
        (echo "hello.baz")))

    $ lake hello:foo
    hello.foo
    hello.bar
    hello.baz

### File

    FILE file-name dependency-list [description] form*

`file` task represents a sequence of operations as well as `task` except that it is executed only when the target file is out of date. `file-name` is a string that specifies the target file's name. `dependency-list` is a list of tasks or file names on which the target file depends. The dependency task/file names are given in both relative and absolute manner, which begins with a colon `:`. `description` is a doc string. `form`s can be any Common Lisp forms.

    $ cat Lakefile
    ...
    (file "hello" ("hello.c")
      "Compile hello from C source code."
      (sh "gcc -o hello hello.c"))

    $ lake hello
    $ ls
    Lakefile hello hello.c

### Directory

    DIRECTORY directory-name [description]

`directory` task represents a task that ensures a directory with name of `directory-name` exists. `description` is a doc string. `directory` task does not depend on other tasks.

    $ cat Lakefile
    ...
    (directory "dir")

    $ lake dir
    $ ls
    Lakefile dir

### Namespace

    NAMESPACE namespace form*

`namespace` groups up multiple tasks for ease of their management. `namespace` is a string that specifies the namespace. Tasks in a namespace are refered with the namespace as prefix separated with a colon `:`. Namespaces may be recursive.

    $ cat Lake
    ...
    (namespace "foo"
      (namespace "bar"
        (task "baz" ()
          (echo "foo.bar.baz")))))

    $ lake foo:bar:baz
    foo.bar.baz

### Task arguments

`task` and `file` task may take task arguments with which users can supply additional information used in task execution. Task arguments are defined as following:

    (task ("hello" first-name last-name) ()
      (echo #?"Hello ${first-name} ${last-name}!"))

Here `hello` task takes two task arguments, `first-name` and `last-name`, and uses them in the task action to echo a line.

Task arguments may have their default value as following:

    (task ("hello" (first-name "john") (last-name "doe") ()
      (echo #?"Hello ${first-name} ${last-name}!"))

To supply task arguments to a task, the task name followed by bracket enclosed string is passed to `lake` function:

    > (lake "hello[john,doe]")
    Hello john doe!

or `lake` command in the command line:

    $ lake hello[john,doe]
    Hello john doe!

If no task argument is supplied, environment variable whose name is the upcase of the name of the task argument is searched and its value is used if it is found. If no such an environment variable, the default value of the task argument is used. If no default value is defined, the task argument has `nil`.

Note that task arguments following the task name does not include spaces because the shell splits the command at the existence of the spaces.

    $ lake hello[john, doe]
    No task "hello[john," found.

If spaces are needed, the task name and following task arguments should be quoted.

    $ lake "hello[billy bob, smith]"
    Hello billy bob smith!

For convenience, if the string supplied to a task argument via a bracket enclosed string or an environment variable is "t", "nil" or their uppercase, it is read to `t` or `nil` and the task argument has the read value. Otherwise, the task argument has the string as it is without being read.

### Lakefile Modularity

Lakefile modularity is quite easy without any special facilities, just `load` a Lakefile from another is enough. Tasks with same name are replaced with newer loaded as ones in a Lakefile. Namespaces with same name are merged into.

Lakefile

    (load "Lakefile.sub")

    (namespace "name"
      (task "foo"
        (echo "name.foo")))

Lakefile.sub

    (namespace "name"
      (task "bar"
        (echo "name.bar")))

So you can execute the two tasks respectively as following.

    $ ls
    Lakefile Lakefile.sub
    $ lake name:foo
    name.foo
    $ lake name:bar
    name.bar

## API

### [Function] lake

    LAKE &key target pathname jobs verbose

Loads a Lakefile specified with `pathname` to execute a task of name `target` defined in the Lakefile. `jobs` as an integer gives how many tasks execute simultaneously. The default number is one, which means serial execution. Not nil `verbose` provides verbose mode. If `target` is not given, `"default"` is used for the default task name. If `pathname` is not given, a file of name `Lakefile` in the current directory is searched for. You should be aware that where the Common Lisp process' current directory is.

    (lake :target "hello")

### [Function] echo

    ECHO string

Writes the given `string` into the standard output followed by a new line, provided for UNIX terminology convenience.

    (task "say-hello" ()
      (echo "Hello world!"))

### [Function] sh

    SH command &key echo

Spawns a subprocess that runs the specified `command` given as a string or list of strings. When `echo` is not `nil`, prints `command` to the standard output before running it. Actually it is a very thin wrapper of `uiop:run-program` provided for UNIX terminology convenience.
Acompanied with cl-interpol's `#?` reader macro, you get more analogous expressions to shell scripts.

    (defparameter cc "gcc")

    (task "hello" ("hello.c")
      (sh #?"${cc} -o hello hello.c"))

### [Function] ssh

    SSH command &key echo

Spawns a subprocess that runs the specified `command`, given as a string or list of strings, on a remote host using `ssh(1)`. `*ssh-host*`, `*ssh-user*` and `*ssh-identity*` should be bound properly before use this. When `echo` is not `nil`, prints `ssh` command published to the standard output before running it.

    (setf *ssh-host* "remotehost")
    (setf *ssh-user* "user")
    (task "hello-via-ssh" ()
      (ssh "echo Hello World!"))

Note that the following binding does not work as intended because the dynamic binding only keep when `task` macro is evaluated, have already exited when `ssh` function is to be actually evaluated.

    ;; Does not work as intended.
    (let ((*ssh-host* "localhost")
          (*ssh-user* "`whoami`"))
      (task "hello-via-ssh" ()
        (ssh "echo Hello World!")))

Instead, the next works as intended. Anyway, the former style with `setf` would be enough in Lakefile.

    ;; Works as intended.
    (task "hello-via-ssh" ()
      (let ((*ssh-host* "localhost")
            (*ssh-user* "`whoami`"))
        (ssh "echo Hello World!")))

### [Special Variable] \*ssh-host\*, \*ssh-user\*, \*ssh-identity\*

These special variables are used to establish a secure connection using `ssh` function. The default value of `*ssh-host*` is unbound so it should be always bound properly when using secure connections. The default value of `*ssh-user*` is `nil`, for giving optional user name. The default value of `*ssh-identity*` is `nil`, for giving optional identity file to prove his/her identity to the remote machine.

### [Function] scp

    SCP from-place pathspec1 to-place pathspec2 &key echo

Copies files between hosts on a network using `scp(1)`. `from-place`, which must be `:local` or `:remote`, specifies if `pathspec1` is a file path on local host or remote host respectively. `pathspec1` is a file path to be copied from, given as a string or a pathname. `to-place` and `pathspec2` are same as `from-place` and `pathspec1` except that they are about files to be copied to.

As `ssh` function above, `*ssh-host*`, `*ssh-user*` and `*ssh-identity*` should be bound properly before use this. When `echo` is not `nil`, prints `scp` command published to the standard output before running it.

    (setf *ssh-host* "remotehost")
    (setf *ssh-user* "user")
    (task "scp" ()
      "Copy ~/foo on local host to ~/foo on remote host."
      (scp :local "~/foo" :remote "~/foo"))

### [Function] execute

    EXECUTE target

**DEPRECATED**

Executes a task specified with `target` as a string within another. The name of the target task is resolved as well as `task` macro's dependency list in both relative and absolute manner.

    (task "hello" ("hello.c")
      (sh "gcc -o hello hello.c")
      (execute "ls"))

    (task "ls" ()
      (sh "ls -l"))

### [Function] display-tasks

    DISPLAY-TASKS &key pathname verbose

Displays the tasks with descriptions in a Lakefile specified with `pathname`. Not nil `verbose` provides verbose mode. If `pathname` is not given, a file of name `Lakefile` in the current direcotry is searched for. You should be aware that where the Common Lisp process' current directory is.

    (display-tasks)

## Command Line Interface

### lake

Lake provides its command line interface as a roswell script.

    SYNOPSIS

        lake [ -f lakefile ] [ options ] ... [ targets ] ...

    OPTIONS

        -f FILE
            Use FILE as a Lakefile.
        -h
            Print usage.
        -j INTEGER
            Execute multiple tasks simultaneously.
        -T
            Display the tasks with descriptions, then exit.
        -v
            Verbose mode.

    EXAMPLE

        $ lake hello:foo hello:bar

### lake-tools

Lake also provides `lake-tools` command as a roswell script which is a complementary program to provide some useful goodies.

Here shows `lake-tools` command's detail. `lake-tools init` command would be replaced by roswell's `ros init` facility (See issue #12).

    SYNOPSIS

        lake-tools COMMAND

    COMMANDS

        dump    Prepare LAKE command to make it run much faster.
        init    Create an empty Lakefile with boilerplates in current directory.

    EXAMPLE

        $ lake-tools init
        $ ls
        Lakefile

## Brief History

Originally lake was called clake, and [@Rudolph-Miller](https://github.com/Rudolph-Miller) gave its name and concept on his GitHub repository. Then, [@takagi](https://github.com/takagi) forked it to design, implement, test, document and CI independently. Afterwards it is renamed to lake.

## Author

* Rudolph Miller (chopsticks.tk.ppfm@gmail.com)
* Masayuki Takagi (kamonama@gmial.com)

## Copyright

Copyright (c) 2015 Rudolph Miller (chopsticks.tk.ppfm@gmail.com)

## License

Licensed under the MIT License.
