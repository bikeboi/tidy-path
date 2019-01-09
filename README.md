# Tidy-Path

> A PATH cleaning utility with a very uninspired name

It cleans your PATH variable.

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

Current functionality:
## Simple Clean
Removes all redundant entries in your PATH
