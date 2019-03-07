% Robert Sato (rssato@ucsc.edu)
% Huanlei Wu (hwu43@ucsc.edu)

not( X ) :- X, !, fail.
not( _ ).

fly( from, to ) :-
   isFlight( from, to).

% sees if there is a viable path
isFlight( from, to ) :-
   flight( from, to, _, _ ).

isFlight( from, to ) :-
   flight( from, layover ), isFlight( layover, to ).

% calculate the time
