# compiler

This is a simple compiler for PA037 course.

You can compile it with [Nix](https://nixos.org/nix/) by running

```
nix build
```

then build one of the example programs with

```
result/bin/compiler examples/fibonacci.lang -o fibonacci
./fibonacci 200
```

The compiler uses `llc` to convert the generated LLVM IR to assembly, and then Clang to build an executable and link it. You can stop at any stage and view the intermediate build product, see `compiler --help` for the list of flags.

## References
* https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl01.html
* https://github.com/sdiehl/kaleidoscope/tree/master/src/chapter7
* https://llvm.org/docs/LangRef.html#functions
* https://hackage.haskell.org/package/llvm-hs
* https://hackage.haskell.org/package/megaparsec
* https://mmhaskell.com/blog/2018/2/26/attoparsec-the-clarity-of-do-syntax
* https://hackage.haskell.org/package/tasty
* https://www.reddit.com/r/haskell/comments/4x22f9/labelling_ast_nodes_with_locations/
