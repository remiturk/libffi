# `libffi` examples

This directory contains a variety of small examples to demonstrate various
aspects of the `libffi` library. We recommend using the `Makefile` to build
each example so that any shared libraries that the example depends on are also
built.

```
$ make CTime
```

After building an example, use `cabal run` to invoke the example, e.g.,

```
$ cabal run CTime
```

## Limitations

The examples currently only work on Linux. See
[issue #4](https://github.com/remiturk/libffi/issues/4).
