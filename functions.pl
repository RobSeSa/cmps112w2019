% Robert Sato (rssato@ucsc.edu)
% Huanlei Wu (hwu43@ucsc.edu)

not( X ) :- X, !, fail.
not( _ ).

fly( From, To ) :-
   isFlight( From, To, [From] ).

% sees if there is a viable path
isFlight( From, To , Tried ) :-
   flight( From, To, _ ),
   append( Tried, [To], Path ), 
   printSched( Path ).
   %nl, write( From ), write(' '), write( To ).

isFlight( From, To , Tried ) :-
   not( member( To, Tried ) ),
   flight( From, Layover, _ ), 
   not( member( Layover, Tried ) ),
   isFlight( Layover, To, [Layover|Tried] ).
   %nl, write( From ), write(' '), write( Layover ).

% calculate the time
%radians( From, To ) :-
   %airport( From, _, degmin( Deg1, Min1 ), degmin( Deg2, Min2 ) ),
   %airport( To, _, degmin( Deg3, Min3 ), degmin( Deg4, Min4 ) ),
   %Rad1 is (Deg1 + Min1 div 60) + pi div 180,
   %write( Rad1 ).
   %rad2 is ,
   %rad3 is ,
   %rad4 is , 

printSched( [Head] ) :-
   write( Head ).

printSched( [Head|Tail] ) :-
   write( Head ), nl,
   printSched( Tail ).
