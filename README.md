# type-level-integers [![Hackage version](https://img.shields.io/hackage/v/type-level-integers.svg?label=Hackage)](https://hackage.haskell.org/package/type-level-integers) [![Stackage version](https://www.stackage.org/package/type-level-integers/badge/lts?label=Stackage)](https://www.stackage.org/package/type-level-integers) [![Build Status](https://travis-ci.org/mtesseract/type-level-integers.svg?branch=master)](https://travis-ci.org/mtesseract/type-level-integers)

This Haskell package implements naive type level integers. It exposes
the module `Data.Type.Integer` which exports a new kind `LiftedInt`
populated by the types `Z` (zero) and `LInt Sign PosNat`. In other
words, a (type level) integer is either zero or a positive natural
number together with a sign.

The module exports the type families `LIntSucc`, `LIntPred`,
`LIntInvert`, `LIntPlus` and `LIntMinus` for manipulating types of
kind `LiftedInt`.
