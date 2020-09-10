# usfx.sudoku
```
$ git clone https://github.com/usfx/usfx.sudoku.git

$ cd sudoku
sudoku$
```

This is a Cabal project. Use `cabal` to compile the code and run the tests:

```
sudoku$ cabal configure
sudoku$ cabal run test-sudoku
--- output of tests
```

The easiest way to experiment with the code whilst you work on it is 
by running `cabal repl` from the top level of the project:

```
$ cd sudoku 
$ cabal repl
```

Sudoku is a logic puzzle from Japan which gained popularity in the West during the 90s. Most newspapers now publish a Sudoku puzzle for the readers to solve every day.

A Sudoku puzzle consists of a 9x9 grid. Some of the cells in the grid have digits (from 1 to 9), others are blank. The objective of the puzzle is to fill in the blank cells with digits from 1 to 9, in such a way that every row, every column and every 3x3 block has exactly one occurrence of each digit 1 to 9.
