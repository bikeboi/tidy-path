# Tidy-Path

> A PATH cleaning utility with a very uninspired name

It removes redundant entries in your `PATH`. Future additions may check for
unused entries maybe, who knows?

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
