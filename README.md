# About

In https://github.com/dbalchev/advent-2023 I tried to solve Advent of Code 2023 in Haskell, but iI kinda failed.
My hypothesis is that I tried to write an Imperative implementation (like the one I can write in Python) 
using the Haskell syntax, which is really suboptimal. Another mistake I made is to use a plethora of libraries
in an ad-hoc manner.

In this repo I'll try to solve Advent of Code 2020, but instead I'll try to stick to a more idiomatic Haskell code.

I'll try to keep the following styleguide to simplify the code and keep me focused on the task instead of fighting 
with Haskell:
1. I will NOT use impure logic, unless strictly necessary (e.g. readding files). I'll try to stick to the pure core
   with an IO shell for reading the input. This will help me focus on Haskell's strengths.
1. I will stick to a small set of **persistent** lazy data structures, without close alternatives
   (e.g. frozen vs mutable or strict vs lazy). I need the persistent part, so I won't worry excesively about performance.
1. I will write a small library (AocPrelude), that will streamline the interface (e.g. a general fromList function,
   so no need to write Text.fromList vs HashMap.fromList). 
   1. I will write the AocPrelude as needed, since there are a lot of functions that need need to be generalized this way.