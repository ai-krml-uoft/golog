# Golog Interpreters

This repo contains various interpreters for the Golog high-level agent programming language, as developed in the Cognitive Robotics Group at University of Toronto and reported in the following book:


* Raymond Reiter, Knowledge in Action: [Logical Foundations for Specifying and Implementing Dynamical Systems](https://direct.mit.edu/books/monograph/2080/Knowledge-in-ActionLogical-Foundations-for). MIT Press, 2001.

The code here has been ported to [SWI-Prolog](https://www.swi-prolog.org/).

## Golog

File [golog.pl](golog.pl) contains the vanilla Golog as per the book but ported to SWI-Prolog. It only contains two simplifications:

- predicate `restore_situation/3` to restore the situation term as the last argument in fluents is now domain independent, so it does not need to be specified per fluent.
- predicate `is_atom/1` has been refactored to be based on `is_complex/1`.

To test the interpreter, we can run it on the simple elevator example as follows:

```prolog
$ swipl golog.pl simple_elevator.pl
Welcome to SWI-Prolog (threaded, 64 bits, version 9.2.5)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- do(control, s0, S).
S = do(open, do(down(0), do(close, do(open, do(turnoff(5), do(up(5), do(close, do(open, do(turnoff(...), do(..., ...)))))))))) [write]
S = do(open, do(down(0), do(close, do(open, do(turnoff(5), do(up(5), do(close, do(open, do(turnoff(3), do(down(3), s0)))))))))) .
```