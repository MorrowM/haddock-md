# haddock-md: A GHC plugin to use markdown in haddock documentation, powered by pandoc

## Usage

1. Install [pandoc](https://pandoc.org/). Make sure it's on your `PATH`.
2. Add this package as a dependency.
3. Add the following to the top of your module:

```hs
{-# OPTIONS_GHC -fplugin=HaddockMd #-}
```

You can also enable it for the entire package (using cabal as an example):

```plain
ghc-options: -fplugin=HaddockMd
```

You can also choose a different format that pandoc supports reading:

```plain
-fplugin=HaddockMd:rst
```
