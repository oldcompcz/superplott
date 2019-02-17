# Superplott

Driver for XY4150 plotter and ZX Spectrum.

## Project structure

* `/bin` - contains original binary
* `/docs` - contains documentation
* `/src` - contains disassembled source - **IN PROGRESS** 

## Documentation

* [XY4150_Navod](docs/XY4150_Navod.pdf)
* [Testing program in BASIC](docs/Testovaci_program_MS-SP.png), here is [result](docs/XY4131-TEST.jpeg).

## How to load, init and use

```basic
CLEAR 60499
LOAD "" CODE 60500
RANDOMIZE USR 60500
```

Usage is described in [XY4150_Navod](docs/XY4150_Navod.pdf). 

## Tools

* [z88dk](https://www.z88dk.org)
* [z80dasm](https://www.tablix.org/~avian/blog/articles/z80dasm/)
