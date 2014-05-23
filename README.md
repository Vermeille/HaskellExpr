HaskellExpr
===========

This is a simple and stupid interpreter. I stopped working on it
in favor of http://bitbucket.org/Vermeille/Functionality which
will include an assembler, so this is kind of redundant.

Here is an example of what you can type in the program:

    a = 10
    b = 42
    c = a * b
    %add(lhs, rhs) lhs + rhs
    c = add(a, b)

It sucks, I can go way further, but it takes time. It was challenging
for me as a first Haskell project.
