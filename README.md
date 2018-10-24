
This is a fork of the text and exercises for the PureScript book written by Phil Freeman

The repo on which this is based is the content behind the "PureScript by Example" book, available on Leanpub: https://leanpub.com/purescript
Its license on Leanpub is [CreativeCommons BY-NC-SA](https://creativecommons.org/licenses/by-nc-sa/3.0/deed.en_US), which is a license allowing remixes for non-commercial purposes with attribution to the original work. This license should permit this fork to exist and be maintained.

# Changes
- migrate to purescript 0.12 and use psc-package
  - thanks to https://github.com/paf31/purescript-book/pull/135
- a seperated branch includes my own solution of the book exercise


# Usage

1. Prerequisites
  - install `purescript`
    - https://github.com/purescript/documentation/blob/master/guides/Getting-Started.md
  - install `psc-package` 
    - https://psc-package.readthedocs.io/en/latest/installation.html

2. clone the repository
   ```sh
   # pwd: ~/
   $ git clone git@github.com:conradlo/purescript-book.git
   ```
3. `cd` to one of the source code folders and install dependency
   ```sh
   # pwd: ~/purescript-book/exercise/chapter{x}
   $ psc-package install
   ```
4. Verify you can open a `repl` or run test successfully.
   
   ```sh
   # open repl
   $ psc-package repl
   # or
   $ pulp --psc-package repl

   # run tests
   $ pulp --psc-package test
   ```

# Links
https://github.com/paf31/purescript-book  
https://leanpub.com/purescript
