This is a "quick and dirty" interpreter for a Scheme-ish language in Haskell, mostly based off of Peter Michaux's article ["Scheme From Scratch."](http://michaux.ca/articles/scheme-from-scratch-introduction) It's my way of getting better with Haskell while simultaneously learning about Scheme & Lisp See `Prelude.hscm` and `Test.hscm` for usage.

## Install & Run ##

1. install [Parseclone](http://github.com/bonasaurus1/Parseclone)
2. download hsscheme source, unzip, and cd into extracted directory
3. `$ sudo cabal install` (note: the dependency tree is pretty big, because of the `view` experimental feature -- see below)
4. create `/usr/lib/hsscheme/` and move `Prelude.hscm` there
5. `$ hsscheme` (for REPL) or `$ hsscheme myfile.hscm` (to run file)

Developed on Ubuntu and OS X with GHC 6.12.1.

## Experimental Feature: View ##

The function `view` visualizes data structures using [Ubigraph](http://ubietylab.net/ubigraph/index.html), via [vacuum-ubigraph](http://hackage.haskell.org/package/vacuum-ubigraph). Usage:

1. install ubigraph (see [site](http://ubietylab.net/ubigraph/index.html))
2. start server: `$ ubigraph_server`
3. in hsscheme: `>> (view [any value])`

This uses [vacuum](http://hackage.haskell.org/package/vacuum-1.0.0) to display the Haskell data structures (`SchemeObj` in `Language/Scheme/Model.hs`) representing Scheme objects. Try visualizing a non-builtin function (just `(view myfunc)`) -- it'll visualize the data that is the code for that function. Not very useful, but pretty neat.

## TODO ##

* lambdas (closures)
* tracebacks
* source locations for error messages
* a `catch` form
* fail gracefully when `ubigraph_server` isn't running and `view` is called
