# `libffi`
[![Hackage](https://img.shields.io/hackage/v/libffi.svg)][Hackage: libffi]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/libffi.svg)](http://packdeps.haskellers.com/reverse/libffi)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Linux build](https://github.com/remiturk/libffi/workflows/Haskell-CI/badge.svg)](https://github.com/remiturk/libffi/actions?query=workflow%3AHaskell-CI)

[Hackage: libffi]:
  http://hackage.haskell.org/package/libffi
  "libffi package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

A binding to `libffi`, allowing C functions of types only known at runtime to be called from Haskell.

# Notes on static linking

This library makes the somewhat unusual choice to link against `libffi`
statically by default rather than dynamically. This is because GHC bundles its
own dynamically linked `libffi`, and moreover, there is no guarantee that the
version of `libffi` that GHC bundles will match the version of `libffi` that
your operating system provides. When these versions don't match, any executable
that depends on the `libffi` library will likely crash at runtime with an error
message like this:

```
error while loading shared libraries: libffi.so.7: cannot open shared object file: No such file or directory
```

For more information, see
[GHC#15397](https://gitlab.haskell.org/ghc/ghc/-/issues/15397). We work around
this issue by forcing the use of static linking in the `libffi` library (using
`-fstatic` flag) on operating systems that support it. If you really want to
dynamically link against `libffi`, compile this library with `-f-static`.

Note that the `-fstatic` flag is not supported on macOS because macOS's linker
does not support either `-Bstatic -lffi `-Bdynamic` or `-l:libffi.a`, which are
the only two ways that we are aware of for forcing the use of static linking
without hard-coding the full path to `libffi.a`. It turns out that `-fstatic`
is not even required on macOS since macOS will statically link against `libffi`
for you, even if you do not explicitly ask for it. We are not quite sure why
this happens (see [issue #1](https://github.com/remiturk/libffi/issues/1) for
more on this), but it has the fortunate side effect of avoiding GHC#15397
altogether on macOS.
