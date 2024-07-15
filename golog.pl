/*--------------------------------------------------------------------------
  GOLOG Interpreter for SWI-Prolog

  Adapted from Golog for ECLIPSE Prolog from book:

  Raymond Reiter, Knowledge in Action: Logical Foundations for Specifying and Implementing Dynamical Systems. MIT Press, 2001.

  Sebastian Sardina - January 1999 - ssardina@gmail.com
---------------------------------------------------------------------------*/
:- ensure_loaded(utils).

:- multifile
  restore_situation/3,
  primitive_action/1.

/*-------------------------------------------------------------------------
  PREDICATE: Definition of operators
  DESCRIPTION: Operators to be used
--------------------------------------------------------------------------*/
:-op(800, xfy, [&]).	  % Conjunction
:-op(850, xfy, [v]).	  % Disjunction
:-op(870, xfy, [=>]).	  % Implication
:-op(880, xfy, [<=>]).	% Equivalence
:-op(950, xfy, [:]).	  % Action sequence
:-op(960, xfy, [#]).	  % Nondeterministic action choice


/*-------------------------------------------------------------------------
  PREDICATE: Do(P, S, S2)
  DESCRIPTION: Do Macro. Do succedes if S2 is a valid final situation after
		executing P from situation S
--------------------------------------------------------------------------*/
do(E1 : E2, S, S1) :- do(E1, S, S2), do(E2, S2, S1).
do(?(P), S, S) :- 	holds(P, S).
do(E1 # E2, S, S1) :- do(E1, S, S1) ; do(E2, S, S1).
do(if(P, E1, E2), S, S1) :- do(( ?(P):E1 ) # ( ?(-P):E2) , S, S1).
do(star(E), S, S1) :- S1=S ; do(E : star(E), S, S1).
do(while(P, E), S, S1) :- do(star(?(P):E) : ?(-P), S, S1).
do(pi(V, E), S, S1) :- sub(V, _, E, E1), do(E1, S, S1).
do(E, S, S1) :- 	proc(E, E1), do(E1, S, S1).
do(E, S, do(E, S)) :- primitive_action(E), poss(E, S).


/*-------------------------------------------------------------------------
  PREDICATE: sub(Name, New, Term1, Term2)
  DESCRIPTION:  Term2 is Term1 with Name replaced by New
      Term2 = Term1[Name/New]
--------------------------------------------------------------------------*/
sub(_, _, T1, T2) :- var(T1), !, T2 = T1.
sub(X1, X2, T1, T2) :- \+ var(T1), T1 == X1, !, T2 = X2.
sub(X1, X2, T1, T2) :- T1 =.. [F|L1], sub_list(X1, X2, L1, L2), T2 =.. [F|L2].

sub_list(_, _, [], []).
sub_list(X1, X2, [T1|L1], [T2|L2]) :-
  sub(X1, X2, T1, T2),
  sub_list(X1, X2, L1, L2).


/*-------------------------------------------------------------------------
  PREDICATE: holds(Cond, S)
  DESCRIPTION:  The revised Lloyd-Topor transformation on test conditions.
--------------------------------------------------------------------------*/
holds(P & Q, S) :- holds(P, S), holds(Q, S).
holds(P v Q, S) :- holds(P, S); holds(Q, S).
holds(P => Q, S) :- holds(-P v Q, S).
holds(P <=> Q, S) :- holds((P => Q) & (Q => P), S).
holds(-(-P), S) :- holds(P, S).
holds(-(P v Q), S) :- holds(-P & -Q, S).
holds(-(P & Q), S) :- holds(-P v -Q, S).
holds(-(P => Q), S) :- holds(-(-P v Q), S).
holds(-(P <=> Q), S) :- holds(-((P => Q) & (Q => P)), S).
holds(-all(V, P), S) :- holds(some(V, -P), S).
holds(-some(V, P), S) :- \+ holds(some(V, P), S).
holds(-P, S) :- 	is_atom(P), \+ holds(P, S).
holds(all(V, P), S) :- holds(-some(V, -P), S).
holds(some(V, P), S) :- sub(V, _, P, P1), holds(P1, S).


holds(A, S) :- restore_situation(A, S, F), !, F.
holds(A, S) :- \+ restore_situation(A, S, _), is_atom(A), A.


% A is a propositional atom, not a complex formula
is_atom(A) :- \+ is_complex(A).

% term is a complex formula
is_complex(-_W).
is_complex(_W1 & _W2).
is_complex(_W1 => _W2).
is_complex(_W1 <=> _W2).
is_complex(_ v _W2).
is_complex(some(_V, _W)).
is_complex(all(_V, _W)).


% restore S as last argument of term A
restore_situation(poss(A), S, poss(A, S)).

% this will not work, as it will also do replacement say on equality terms
% restore_situation(A, S, A2) :-
%   is_atom(A), A =.. [F|L], append(L, [S], L2), A2 =.. [F|L2].

% is the situation executable -- all actions have poss true
executable(s0).
executable(do(A, S)) :- executable(S), poss(A, S).

