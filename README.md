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
\require "server"

% listen on port 1225 ("ly" in numbers)
#(lys:start)

% to specifiy the listening port:
#(lys:start 12321)
```

Or alternatively, run from the command line:

```bash
$ lilypond -r server -e "#(lys:start)"
```

## Connecting

Commands can be sent to the server by piping to `nc`:

```bash
echo "(lys:compile \"~\" \"myfile.ly\")" | nc localhost 1225
```

## The lys:compile command

The server implementation includes the `lys:compile` command that provides a functionality identical to the lilypond command line interface.

Usage: `lys:compile pwd opt ...`

where `pwd` is the client's working directory, and opt is one or more command line options. `lys:compile` currently handles all of lilypond's command line options except `--loglevel` and `--include`.



See also the included [example client](https://github.com/noteflakes/lyp-server/blob/master/test/lyc.sh).

## Problems & Limitations

- `lys:compile` can't handle --loglevel and --include options.
- This is a proof of concept. It's not very well tested and might not work on anything bigger than 20 bars.
