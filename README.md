# C++ Code Scanner in Haskell

This repository contains the implementation of a C++ code scanner written in Haskell. A scanner is a program that takes a sequence of characters (the source file of the program) and produces a sequence of tokens.

## Introduction

The C++ code scanner is built using the Haskell programming language. It reads a C++ source file and generates a text file containing tokens and lexemes in the format `<token, lexeme>`.

## Specifications

The C++ code scanner follows the following specifications:

1. All numbers (int, float, and double) are heptal (base 7). Any other numbers should be treated as errors.
2. Identifiers must be at least 2 characters long, contain at least one capital letter, and cannot start with a digit.
3. Operators include +, -, *, /, %, <, ≤, >, ≥, =, ==, !=, &&, ||, [, ], (, ), −−, { }, ++, <<, and >>.
   - Operators can be written with or without whitespace around the operands or expressions.
   - Examples of valid operator representations: "a+b", "a + b", " a + b ".
4. Delimiters include ,, ;, and :.
5. Keywords recognized by the scanner are:
   int, float, double, void, while, for, if, else, char, array, struct, class, break, case, return, cout, cin, true, false, endl.
6. Strings are enclosed in inverted commas.
7. Comments can be single-line or multi-line:
   - Single-line comments start with // and end automatically at the end of the line.
   - Multi-line comments start with /* and end with */.

