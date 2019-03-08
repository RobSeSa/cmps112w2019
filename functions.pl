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
   write( '1' ), nl,
   flight( From, Layover, _ ), 
   write( '2' ), nl,
   not( member( Layover, Tried ) ),
   write( '3' ), nl,
   time( From, To, ArriveTime ),
   write( '4' ), nl,
   minutes( From, Layover, FromMin ),
   write( '5' ), write(Layover), write( ' ' ), write(To), nl,
   minutes( Layover, To, ToMin ),
   write( '6' ), nl,
   FinalMin is FromMin + ArriveTime + 30,
   write( '7' ), nl,
   write( FinalMin ), write( ' ' ), write( ToMin ),
   write( '8' ), nl,
   ToMin >= FinalMin,
   isFlight( Layover, To, [Layover|Tried] ).
   %nl, write( From ), write(' '), write( Layover ).

minutes( From, To, Minutes ) :-
   flight( From, To, time( Hour, Min ) ),
   Minutes is Hour * 60 + Min.

% calculates the radians
radians( From, To , Distance) :-
   airport( From, _, degmin( Deg1, Min1 ), degmin( Deg2, Min2 ) ),
   airport( To, _, degmin( Deg3, Min3 ), degmin( Deg4, Min4 ) ),
   Rad1 is (Deg1 + Min1 / 60) * pi / 180,
   Rad2 is (Deg2 + Min2 / 60) * pi / 180,
   Rad3 is (Deg3 + Min3 / 60) * pi / 180,
   Rad4 is (Deg4 + Min4 / 60) * pi / 180,
   haversine_radians( Rad1, Rad2, Rad3, Rad4, Distance ).

% calculates the distance
haversine_radians( Lat1, Lon1, Lat2, Lon2, Distance ) :-
   Dlon is Lon2 - Lon1,
   Dlat is Lat2 - Lat1,
   A is sin( Dlat / 2 ) ** 2
      + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
   Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
   Distance is Dist * 3959.
   %write(Distance).

% calculates the duration of flight
time( From, To, Time ) :-
   radians( From, To, Distance ),
   Hours is Distance / 500,
   Time is round( Hours * 60 ).
   %write( Time ), nl.

printSched( [Head] ) :-
   write( Head ).

printSched( [Head|Tail] ) :-
   nl, write( Head ), nl,
   printSched( Tail ).
