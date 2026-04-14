# HackAssembler_hs

A two-pass assembler for the Hack assembly language, written in Haskell. Takes a Hack assembly file (```.asm```) and converts it to a binary file (```.hack```).

This is the assignment for module 6 of the course [From NAND to Tetris](https://www.nand2tetris.org/).

I've left both the input and output files in the repo.

## How it works

First Pass: builds the symbol table from label and variable declarations

Second Pass: translates the resulting file to binary using the symbol table

### Why Haskell?

The core problem is a mapping between instruction sets. This felt like a natural fit for functional programming, and Haskell was the functional language that I knew.

## How to run

You will need [the Haskell toolchain](https://www.haskell.org/ghcup/) installed on your machine.

```
cabal build
cabal run HackAssembler-hs <input.asm> <output.hack>
```

This was tested using the online IDE associated with the course, [here](https://nand2tetris.github.io/web-ide/asm). Load a file from project 6 into the 'Source' section and convert it to binary using the online IDE. Then run this HackAssembler_hs locally with the same input file. Load the output file (that you've just generated) into the 'Compare Code' section, and click the compare button to verify that the binary files are the same.

## Future Plans

- [ ] potentially make a similar project in C++ and take benchmarks
