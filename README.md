# lys - Run lilypond as a server

This package provides a command for running lilypond as a server.

## Why?

Because sometimes lilypond is just too damn slow...

## Mode of operation

The server.ly script starts a server on port 12321. Clients connect and can pass arbitrary scheme expressions. For example, to typeset a lilypond file, a client can invoke `(lys:compile pwd opts...)` where pwd is the current working directory and opts is a list of command line options:

```scheme
(lys:compile "/home/sharon" "--png" "myfile")
```

## How fast is it?

Small files compile in 0.2-0.3s (on a modern laptop). Generally one can expect to shave 0.5-0.6s off compilation time.

## Problems & Limitations

- `lys:compile` can't handle --loglevel and --include options.
- This is a proof of concept. It's not very well tested and might not work on anything bigger than 20 bars.

## Installation

Install using [lyp](https://github.com/noteflakes/lyp):

```bash
$ lyp install server
```

## Starting a server

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

See also the included [example client](https://github.com/noteflakes/lyp-server/blob/master/test/lyc.sh).

