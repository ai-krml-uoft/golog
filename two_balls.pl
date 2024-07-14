%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Simulation of Two Balls Bouncing between Two Walls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- ensure_loaded(utils).

:- discontiguous poss/2.
:- discontiguous vel/3, pos/3.

% Assume no precondition interaction problem; a concurrent action is poss-
% ible iff it is coherent, and each of its primitive actions is possible.
poss(C, S) :- coherent(C), allPoss(C, S).
allPoss([], _S).
allPoss([A | R], S) :- poss(A, S), allPoss(R, S).

%  Successor state axioms.
pos(B, X, do(C, S)) :- pos(B, X0, S), vel(B, V0, S), start(S, TS),
                    time(C, TC), X is X0 + V0 * (TC - TS).

vel(B, V, do(C, S)) :- vel(B, V0, S), member(bounce(B, _, _), C), V is -V0.
vel(B, V1, do(C, S)) :-
       (member(collide(B, B1, _), C) ; member(collide(B1, B, _), C)),
        \+ member(bounce(B, _, _), C),
        vel(B1, V1, S).
vel(B, V0, do(C, S)) :- vel(B, V0, S),
    \+ member(collide(B, _, _), C),
    \+ member(collide(_, B, _), C),
    \+ member(bounce(B, _, _), C).



%  Initial situation.
start(s0, 0.0).
vel(b1, 10.0, s0).
vel(b2, -5.0, s0).
pos(b1, 0.0, s0).
pos(b2, 120.0, s0).
wallLocation(w1, 0.0).
wallLocation(w2, 120.0).

%  Natural action declarations.
natural(bounce(B, W, _T)) :-
    member(B, [b1, b2]),
    member(W, [w1, w2]).
natural(collide(B1, B2, _T)) :-
    B1 = b1, B2 = b2.

%  Restore suppressed situation arguments.
restore_situation(pos(B, X), S, pos(B, X, S)).
restore_situation(vel(B, V), S, vel(B, V, S)).
restore_situation(lntp(T), S, lntp(S, T)).
restore_situation(setof(X, Generator, Set), S, setof(X, holds(Generator, S), Set)).

%  Action occurrence times.
time(bounce(_B, _W, T), T).   time(collide(_B1, _B2, T), T).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definitions of Complex Control Actions - PROGRAMS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Procedure to compute the executable situation of length N.
proc(executable(N),
      ?(N = 0) #
      pi(t, ?(N > 0 & lntp(t)) :
            pi(a, pi(c, ?(setof(a, natural(a) &
                                   poss(a) & time(a, t), c)) : c))) :
      pi(n, ?(n is N - 1) : executable(n))).

%   Least-natural-time point.
lntp(S, T) :- natural(A), poss(A, S), time(A, T),
             \+ (natural(A1), poss(A1, S), time(A1, T1), T1 < T), !.

%  Action precondition axioms.
poss(bounce(B, W, T), S) :- vel(B, V, S), \+ V = 0, start(S, TS),
        pos(B, X, S), wallLocation(W, D), T is TS + (D - X)/V, T > TS.

poss(collide(B1, B2, T), S) :- vel(B1, V1, S), vel(B2, V2, S), \+ V1 = V2,
                            start(S, TS), pos(B1, X1, S), pos(B2, X2, S),
                            T is TS - (X1 - X2)/(V1 - V2), T > TS.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Utilities.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% top-level application interface
simulate(T) :- do(executable(T), s0, S),
               prettyPrintSituation(S), askForMore.
askForMore :- write('More (y/n)? '), read(n).


