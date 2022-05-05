# Haskell Shoot 'em up

Welcome to the Haskell Shoot 'em up!

![Space Invaders](images/SpaceInvaders-Gameplay.gif)

Use this to quickly get used to the Haskell language and tool ecosystem.

Note: This repo contains (in some cases modified) exercises from ”Programming in Haskell” by Graham Hutton <http://www.cs.nott.ac.uk/~pszgmh/pih.html>. In some cases, the questions from the exercise booklet are also modified so that they can be solved using a computer (by, for instance resolving name clashes with the standard Haskell prelude). 


# Usage (using local clones)

1. Clone this repo onto your local machine using the command `git clone <repo URL>`. You may alternatively download an archive containing the repo using the download button on the main project page on gitlab in case you have problems using git.    
2. Use `stack build` to build the project to make sure that everything compiles before starting.
3. Start!
4. Once you decide to work on a chapter, uncomment the relevant lines in `test/Spec.hs` to enable its automated tests.  
5. The exercises can be found within the `src` directory. Replace occurrences of `undefined` or `???` with your solutions. Make sure to use the templates and names provided. 
6. Use `stack test` to run the included automated tests and take pleasure from seeing them change color from red to green.


Some helpful commands:
- Build the project: `stack build`
- Run all tests: `stack test`
- Run tests in auto-refresh mode: `stack test --file-watch`
- Only run tests for a particular chapter: `stack test --test-arguments='--match "Chapter_X"'` 



