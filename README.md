# JASI
JASI(Just Another Scheme Interpreter) written in Haskell

Even so, it does support following features:

- Lambda Calculus;
- A prelude `prelude.scm` which pre-defined a set of handy functions. 
- An user-friendly REPL, which support arrow keys use. LOL
- Fullly support to binary, oct,dec and hex base number parsing, float number is fine too.
- Implemented `case`,`if` and `cond` three kinds of condtion mechanism.
- Powerful error handling, you can always get hints and specfic lines number if your code went something wrong.
...


## Build

[stack](http://docs.haskellstack.org/en/stable/README/) is required to build the project. Make sure you have stack1.0.0 installed on your computer. then do:

```
stack build
stack exec schemeprj-exe
```

## play with it

```scheme
ready> (load "prelude.scm")
(lambda ("pred" "lst") ...)
ready> (not #f)
#t
ready> (product 1 2 3 4 5)
120
ready> (sum 1 2 3 4 5)
15
ready> ((lambda (x) (product 2 x)) 10)
20
ready> 
```
