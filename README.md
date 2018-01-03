# dfa

[![Build Status](https://travis-ci.org/hnefatl/dfa.svg?branch=master)](https://travis-ci.org/hnefatl/dfa)

A simple DFA demo in Haskell (practising using the `State` monad, continuous integration, and test suites).

![Image of the state transition diagram for the DFA](DFA.png?raw=true "State Transition Diagram")

This repository follows the standard Stack layout:

- `src/` contains the "library" code (the API).
- `app/` contains the "demo executable" code.
- `test/` contains the test suite.

Tests are written using [`Tasty`](https://github.com/feuerbach/tasty) with the `HUnit` and `QuickCheck` providers.

The DFA diagram was designed using [`draw.io`](https://draw.io).