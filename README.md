#  Simplify context-free grammar 

## Description

The program removes unnecessary symbols from the context-free grammar.

## Build

The program can be compiled using the command (the Haskell compiler [GHC](https://www.haskell.org/ghc/) is assumed to be installed on the system):

```bash
make
```

The created binary file `simplify-bkg` can be executed with the command:
```bash
./simplify-bkg < -i | -1 | -2 > [input_file] 
```

Deleting files created by Makefile can be done with:
```bash
make clean
```

## Description of directory hierarchy
- `src/` - The program source code.
- `test/` - Basic tests.


## Program source files description

The program consists of several modules (source files):

- `src/Main.hs` - Main module of the program.
- `src/InputHandler.hs` - A module for processing program input data.
- `src/Parse.hs` - A module for working with strings/lists.
- `src/Validation.hs` - A module for verifying the content of strings/lists.
- `src/SimplificationCore.hs` - A module providing an abstract layer that encapsulates program logic.
- `src/SimplificationLogic.hs` - A module providing the creation of a new context-free grammar - program logic.
- `src/Struct.hs` - A module for structure declarations.



## Note

The resulting order of terminals/non-terminals/rules in the resulting grammar may be in a different order, but their meaning is equivalent.
