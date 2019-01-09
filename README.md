# Tidy-Path

> A PATH cleaning utility with a very uninspired name

It removes redundant entries in your `PATH`. Future additions may check for
unused entries maybe, who knows?

Pre-built releases are available [here](https://github.com/bikeboi/tidy-path/releases), but building
from source is not that much of a hassle.

## Building
The easiest way to build this is using the Haskell `Stack` tool. Which can be installed with the
instructions [here](https://docs.haskellstack.org/en/stable/README/) (It's also very likely that most linux
distributions have an up to date version of it in their package repos).
Once you have stack, navigate to the source directory and run:
```
$ ./build
```
If this fails for any reason make sure you have stack installed and on your path. If this doesn't solve it, make an issue on the issue tracker.

## Usage
Running it with no arguments uses the currently set `PATH` environment
variable. Cleaned paths get output as plain text to stdout. There are
options for stdin input and verbosity.
```
$ echo $PATH
yeet:yeet:another/yeet

$ tidypath
yeet:another/yeet

```
The path it outputs is also sorted alphabetically (feature not a bug, I
promise).
