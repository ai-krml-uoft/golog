# Golog Interpreters

This repo contains various interpreters for the Golog high-level agent programming language, as developed in the Cognitive Robotics Group at University of Toronto and reported in the following book:


* Raymond Reiter, Knowledge in Action: [Logical Foundations for Specifying and Implementing Dynamical Systems](https://direct.mit.edu/books/monograph/2080/Knowledge-in-ActionLogical-Foundations-for). MIT Press, 2001.

The code here has been ported to [SWI-Prolog](https://www.swi-prolog.org/).

## Golog

File [golog.pl](golog.pl) contains the vanilla Golog as per the book but ported to SWI-Prolog.

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
S = do(open, do(down(0), do(close, do(open, do(turnoff(5), do(up(5), do(close, do(open, do(turnoff(3), do(down(3), s0)))))))))) ;
S = do(open, do(down(0), do(close, do(open, do(turnoff(3), do(down(3), do(close, do(open, do(turnoff(5), do(up(5), s0)))))))))) ;
S = do(open, do(down(0), do(close, do(open, do(turnoff(5), do(up(5), do(close, do(open, do(turnoff(3), do(down(3), s0)))))))))) ;
S = do(open, do(down(0), do(close, do(open, do(turnoff(3), do(down(3), do(close, do(open, do(turnoff(5), do(up(5), s0)))))))))) ;
false.
```

## Concurrent Temporal Golog

This extends the standard Golog with time and concurrent actions (Section 7.8 in book). The interpreter extension is in file [golog_temporal.pl](golog_temporal.pl).

A run of the Two Balls example described in Section 7.9.8 is as follows:

```prolog
$ swipl golog_temporal.pl two_balls.pl
Welcome to SWI-Prolog (threaded, 64 bits, version 9.2.5)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- simulate(10).

[[collide(b1,b2,8.0)],[bounce(b2,w2,12.0)],[bounce(b1,w1,24.0),bounce(b2,w1,24.0),collide(b1,b2,24.0)],[bounce(b2,w2,36.0)],[collide(b1,b2,40.0)],[bounce(b1,w1,48.0),bounce(b2,w2,48.0)],[collide(b1,b2,56.0)],[bounce(b2,w2,60.0)],[bounce(b1,w1,72.0),bounce(b2,w1,72.0),collide(b1,b2,72.0)],[bounce(b2,w2,84.0)]]
More (y/n)? y.

false.
```