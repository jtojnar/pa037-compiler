# compiler

This is a simple compiler for PA037 course.

You can compile it with [Nix](https://nixos.org/nix/) by running

    nix build

 then build one of the example programs with `result/bin/compiler examples/fibonacci.lang`

## References
* https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl01.html
* https://hackage.haskell.org/package/llvm-hs
* https://hackage.haskell.org/package/megaparsec
* https://mmhaskell.com/blog/2018/2/26/attoparsec-the-clarity-of-do-syntax
* https://hackage.haskell.org/package/tasty
* https://www.reddit.com/r/haskell/comments/4x22f9/labelling_ast_nodes_with_locations/
