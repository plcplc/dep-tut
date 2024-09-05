# Experiments in implementing dependent types.

This repo hosts experimentations implementing a dependently typed lambda calculus following the tutorial https://www.andres-loeh.de/LambdaPi/LambdaPi.pdf.

Some exploration topics include:
* Using dependently typed lambda calculus as an application scripting language
* Implementing a structured editor for terms using FRP.

## Running the editor

To get a small terminal-based structured editor application that edits the file `mala.cbor`:

```
$ nix develop
$ cabal run mala
```
