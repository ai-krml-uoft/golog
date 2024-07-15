/*--------------------------------------------------------------------------
  CONCURRENT TEMPORAL GOLOG Interpreter for SWI-Prolog

  A Prolog Interpreter for Concurrent, Temporal Golog

  Adapted from Golog for ECLIPSE Prolog from book:

  Raymond Reiter, Knowledge in Action: Logical Foundations for Specifying and Implementing Dynamical Systems. MIT Press, 2001.

  Sebastian Sardina - January 1999 - ssardina@gmail.com
---------------------------------------------------------------------------*/
:- ensure_loaded(utils).
:- use_module(library(clpfd)).

:- multifile
  time/2,
  start/2,
  do/3.
:- ensure_loaded('golog.pl').

% for sequential temporal golog, set the constraint that the time of action A
% executed in situation S is after the time of S
%   this rule is called by do/2 for primitive actions in core GOLOG
set_action_temporal_constraint(A, S) :-
  start(S, T1), time(A, T2), T1 #=< T2.


do(C, S, do(C, S)) :-  % section 7.8 - page 169
  concurrent_action(C), poss(C, S),
  start(S, T1), time(C, T2), T1 =< T2.


/* The time of a concurrent action is the time of its first simple
   action. This assumes that the concurrent action is coherent, so it
   is non-empty, and all its simple actions occur at the same time.  */
time([A | _], T) :- time(A, T).

% The start time of situation do(C, S) is the time of C.
start(do(A, _S), T) :- primitive_action(A), !, time(A, T).
start(do(C, _S), T) :- concurrent_action(C), time(C, T).


%  Concurrent actions are Prolog lists.
concurrent_action(L) :- is_list(L).

%  Coherent actions defined.
coherent([A | R]) :- time(A, T), sameTime(R, T).
sameTime([], _T).
sameTime([A | R], T) :- time(A, T), sameTime(R, T).
