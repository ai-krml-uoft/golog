%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sequential Temporal Golog Program for a Coffee Delivery Robot
%
%     Section 7.4.1 - page 159
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- ensure_loaded(utils).

:- discontiguous
   robotLocation/2,
   holdingCoffee/2.

% Preconditions for Primitive Actions.
poss(pickupCoffee(_T), S) :-
   \+ holdingCoffee(S), robotLocation(cm, S).

poss(giveCoffee(Person, _T), S) :-
   holdingCoffee(S), robotLocation(office(Person), S).

poss(startGo(Loc1, Loc2, _T), S) :-
   \+ going(_, _, S), \+ Loc1 = Loc2, robotLocation(Loc1, S).
poss(endGo(Loc1, Loc2, _T), S) :- going(Loc1, Loc2, S).

% Successor State Axioms.
hasCoffee(Person, do(A, S)) :-
   A = giveCoffee(Person, _T) ; hasCoffee(Person, S).
robotLocation(Loc, do(A, S)) :-
   A = endGo(_, Loc, _T) ; (robotLocation(Loc, S), \+ A = endGo(_, _, _T)).
going(Loc1, Loc2, do(A, S)) :-
   A = startGo(Loc1, Loc2, _T) ; (going(Loc1, Loc2, S), \+ A = endGo(Loc1, Loc2, _T)).
holdingCoffee(do(A, S)) :-
   A = pickupCoffee(_T) ; (holdingCoffee(S), \+ A = giveCoffee(_Person, _T)).



% Initial Situation.
robotLocation(cm, s0).
start(s0, 0).
wantsCoffee(sue, 140, 160).
% wantsCoffee(bill, 100, 110).
% wantsCoffee(joe, 90, 100).
% wantsCoffee(mary, 130, 170).

travelTime0(cm, office(sue), 15).
travelTime0(cm, office(mary), 10).
travelTime0(cm, office(bill), 8).
travelTime0(cm, office(joe), 10).

travelTime(L, L, 0).
travelTime(L1, L2, T) :- travelTime0(L1, L2, T) ;
                       travelTime0(L2, L1, T).

% The time of an action occurrence is its last argument.
time(pickupCoffee(T), T).
time(giveCoffee(_Person, T), T).
time(startGo(_Loc1, _Loc2, T), T).
time(endGo(_Loc1, _Loc2, T), T).

% Restore situation arguments to fluents.
restore_situation(robotLocation(Rloc), S, robotLocation(Rloc, S)).
restore_situation(hasCoffee(Person), S, hasCoffee(Person, S)).
restore_situation(going(Loc1, Loc2), S, going(Loc1, Loc2, S)).
restore_situation(holdingCoffee, S, holdingCoffee(S)).

% Primitive Action Declarations.
primitive_action(pickupCoffee(_T)).
primitive_action(giveCoffee(_Person, _T)).
primitive_action(startGo(_Loc1, _Loc2, _T)).
primitive_action(endGo(_Loc1, _Loc2, _T)).

% Fix on a solution to the temporal constraints.

chooseTimes(S)  :-
   collect_time_vars(S, T),
   setof(min(X), member(X, T), MinVars),
   once(labeling(MinVars, T)).

collect_time_vars(s0, []).
collect_time_vars(do(A, S), [T|L]) :- time(A, T), collect_time_vars(S, L).



% "now" is a synonym for "start".
now(S, T) :- start(S, T).
restore_situation(now(T), S, now(S, T)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definitions of Complex Control Actions - PROGRAMS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
proc(goFloor(N), ?(currentFloor(N)) # up(N) # down(N)).


proc(deliverCoffee(T),
  ?(some(t, now(t) & t #=< T)) :
  (?(all(p, all(t1, all(t2, wantsCoffee(p, t1, t2) => hasCoffee(p)))))
   #
   pi(rloc, ?(robotLocation(rloc)) :
            if(rloc = cm, /* THEN */ deliverOneCoffee(T),
                          /* ELSE */
                          goto(cm, T) :
                          pi(t, ?(now(t)) : deliverOneCoffee(t))) :
            pi(t, ?(now(t)) : deliverCoffee(t)))   % keep delivering
   )
).

proc(deliverOneCoffee(T),
  pi(p, pi(t1, pi(t2, pi(wait, pi(travTime,
     ?(wantsCoffee(p, t1, t2) &
      -hasCoffee(p) &
      wait #>= 0 &
      travelTime(cm, office(p), travTime) &
      t1 #=< T + wait + travTime &
      T + wait + travTime #=< t2
      ) :
     pi(t, ?(t #= T + wait) : pickupCoffee(t)) :
     pi(t, ?(now(t)) : goto(office(p), t)) :
     pi(t, ?(now(t)) : giveCoffee(p, t))
   )))))
).


proc(goto(L, T),
  pi(rloc, ?(robotLocation(rloc)) : pi(deltat, ?(travelTime(rloc, L, deltat)) :
     goBetween(rloc, L, deltat, T)))
).

proc(goBetween(Loc1, Loc2, Delta, T),
   startGo(Loc1, Loc2, T) :
   pi(t, ?(t #= T + Delta) : endGo(Loc1, Loc2, t))
).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Utilities.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% top-level procedure for coffee delivery.
coffeeDelivery(T) :-
   do(deliverCoffee(T), s0, S),
   chooseTimes(S),
   prettyPrintSituation(S),
   askForMore.
askForMore :- write('More? '), read(n).


