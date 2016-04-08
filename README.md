# lys - Run lilypond as a server

This package provides a command for running lilypond as a server.

## Why?

Because sometimes lilypond is just too damn slow...

## Mode of operation

The listens on port 1225 (by default). Clients connect and pass arbitrary scheme expressions. For example, to typeset a lilypond file, a client can send `(lys:compile pwd opts...)` where pwd is the current working directory and opts is a list of command line options:

```scheme
(lys:compile "/home/sharon" "--png" "myfile")
```

## How fast is it?

Small files compile in 0.2-0.3s (on a modern laptop). Generally one can expect to shave 0.5-0.6s off compilation time.

## Installation

Install using [lyp](https://github.com/noteflakes/lyp):

```bash
$ lyp install server
```

## Starting a Server

Run the following lilypond script:

```lilypond
\require "lys"

% listen on port 1225 ("ly" in numbers)
#(lys:start-server)

% to specifiy the listening port:
#(lys:start-server 12321)
```

Or alternatively, run from the command line:

```bash
$ lilypond -r lys -e "#(lys:start-server)"
```

## Connecting

Commands can be sent to the server by piping to `nc`:

```bash
echo "(lys:compile-file \"~\" \"myfile.ly\")" | nc localhost 1225
```

## API

See also the included [example client](https://github.com/noteflakes/lyp-server/blob/master/test/lyc.sh).

### lys:close

Usage: `lyp:close`

Normally, a connection to the server stays open until the client disconnects. A client connecting through `nc` can ask the server to shutdown the connection by sending `(lyp:close)`.

### lys:compile-file

Usage: `lys:compile-file pwd opt ...`

Compile a lilypond file, where `pwd` is the client's working directory, and opt is one or more [lilypond command line options](http://lilypond.org/doc/v2.18/Documentation/usage/command_002dline-usage.en.html). `lys:compile-file` currently handles all of lilypond's command line options except `--loglevel` and `--include`.

Example: `(lys:compile-file "/Users/dudu" "--png" "myfile.ly")`

### lys:stdin-eval-loop

Usage: `lys:stdin-eval-loop`

Start an evaluate loop on stdin. This can be used to start a long-running slave lilypond process that evaluates arbitrary scheme expressions received on stdin.

### lys:spawn

Usage: `lys:spawn expr ...`

Fork and evaluate a sequence of expressions in the child process. Returns the child pid.

Example: `(lys:spawn (lys:compile-file "/Users/dudu" "--png" "myfile.ly"))`

### lys:typeset-music-slice

Usage: `lys:typeset-music-slice music start-moment end-moment filename`

Compile a range of the given music variable between two moments and output to the given filename. The moments are specified as lists that are converted into ly:moment.

Example: `(lys:typeset-music-slice myMusic '(3 1) '(6 1) "music3-6")`

