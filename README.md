# Exercises in EOPL

Notes and selected exercises for [Essentials of Programming Languages: 3rd edition](http://www.eopl3.com/).

## 1 Inductive Sets of Data

The first chapter is about fundamentals of recursion and induction. The exercises are a great way to help familiarize with Scheme (in my case, Racket). Some interesting notes:

**CFG**
> These grammars are said to be *context-free* because a rule defining a given syntactic category may be applied in any context that makes reference to that syntactic category.

Note that BST is a perfect example of being not context-free, where the *production* of BST's syntactic category doesn't necessarily indicate its correctness.

> Context-sensitive constraints also arise when specifying the syntax of programming languages. For instance, in many languages every variable must be declared before it is used. 

**Follow the Grammar!**
> When defining a procedure that operates on inductively defined data, the structure of the program should be patterned after the structure of the data.

An example is *S-list*, which appears at the beginning in [`exercise-01.rkt`](https://github.com/EDFward/eopl-exercise/blob/master/exercise-01.rkt). `subst` and `subst-in-s-exp` are mutually recursive but guaranteed to terminate since they strictly follow the definition of *S-list* and *S-exp*.

