# compiler

This is a simple compiler for PA037 course.

You can compile it with [Nix](https://nixos.org/nix/) by running

    nix run nixpkgs.cabal2nix -c cabal2nix --hpack --shell . > shell.nix
    nix-build shell.nix

 then build one of the example programs with `result/bin/compiler examples/fibonacci.lang`

## References
* https://llvm.org/docs/tutorial/LangImpl01.html
* https://hackage.haskell.org/package/llvm-hs
* https://hackage.haskell.org/package/megaparsec
* https://mmhaskell.com/blog/2018/2/26/attoparsec-the-clarity-of-do-syntax
* https://hackage.haskell.org/package/tasty