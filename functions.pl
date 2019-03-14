% Robert Sato (rssato@ucsc.edu)
% Huanlei Wu (hwu43@ucsc.edu)

not( X ) :- X, !, fail.
not( _ ).

% returns the first element in the list
pop( [Head|_], Value ) :-
   Value = Head.

% returns the second element in the list
pop2( [_|Tail], Value ) :-
   pop( Tail , Value ).

% sees if there is a viable Path
fly( From, To ) :-
   isFlight( From, To, [From], [From], 0 ).

% airport is not in database.pl
isFlight( From, To, _, _, _ ) :-
   ( not( flight( From, _, _ ) );
   not( flight( _, To, _ ) ) ),
   write( 'Error: one or more of the airports not in database' ),
   nl, !, fail.

% if flight( From -> To ) true
isFlight( From, To, Tried, _, Current ) :-
   flight( From, To, time( Hour, Min ) ),
   getMinutes( Hour, Min, Time ),
   Time >= Current,
   append( Tried, [To], Path ), 
   nl, printSched( Path , 0).

% there is a layover in the flight
isFlight( From, To, Tried, Flights, Current ) :-
   not( member( To, Tried ) ),
   pop( Tried, Head ),
   Head \= From,
   flight( From, Layover, time( Hour, Min) ), !,
   getMinutes( Hour, Min, Time ),
   Time >= Current,
   %write('To '), write(To), nl,
   %write('From '), write(From), nl,
   %write('Layover '), write(Layover), nl,
   %write(Tried), nl, !,
   not( member( Layover, Tried ) ),
   pop2( Flights, PrevFlight ),
   %write('PreFlight '), write(PrevFlight), nl,
   %write(Tried),nl,
   checkTime( PrevFlight, From, Layover ),
   append( Tried, [Layover], Path ),
   %write(Path), nl,
   time( From, Layover, Duration ),
   CurrentNow is Current + 30 + Duration,
   isFlight( Layover, To, Path, [Layover|Flights], CurrentNow ).

% there is a layover in the flight
% setting up for the next isFlight expression
isFlight( From, To, Tried, Flights , Current ) :-
   not( member( To, Tried ) ),
   flight( From, Layover, time( Hour, Min ) ),
   getMinutes( Hour, Min, Time ),
   Time >= Current,
   append( Tried, [Layover], Path ),
   %write('To '), write(To), nl,
   %write('From '), write(From), nl,
   %write('Layover init '), write(Layover), nl,
   %write(Path), nl,
   time( From, Layover, Duration ),
   CurrentNow is Current + 30 + Duration,
   isFlight( Layover, To, Path, [Layover|Flights], CurrentNow ).

% checks if the flight From to Layover makes it before Layover to To
checkTime( From, Layover, To ) :-
   minutes( From, Layover, DepartTimeA ),
   %write( From ), write(' to '), write(Layover), write(' is at '), write(DepartTimeA), nl,
   time( From, Layover, TripDuration ),
   %write('takes this long: '), write(TripDuration), nl,
   minutes( Layover, To, DepartTimeB ),
   %write( Layover ), write(' to '), write(To), write(' is at '), write(DepartTimeB), nl,
   AToB is DepartTimeA + TripDuration + 30,
   %write(AToB), nl,
   AToB =< DepartTimeB.

% converts hours and minutes to minutes
minutes( From, To, Minutes ) :-
   flight( From, To, time( Hour, Min ) ),
   Minutes is Hour * 60 + Min.

getMinutes( Hours, Mins, Sum ) :-
   Sum is Hours * 60 + Mins.

% calculates the duration of flight
time( From, To, Time ) :-
   radians( From, To, Distance ),
   Hours is Distance / 500,
   Time is round( Hours * 60 ).

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

% grabs hours and minutes from a flight
hoursMins( From, To, Hours, Minutes ) :-
   flight( From, To, time( Hour, Min ) ),
   Hours is Hour,
   Minutes is Min.

% converts minutes into hours and minutes
convertTime( Minutes, HoursO, MinsO ) :-
   HoursO is Minutes div 60,
   MinsO is Minutes mod 60.

% changes all characters in string to uppercase
to_upper( Lower, Upper) :-
   atom_chars( Lower, Lowerlist),
   maplist( lower_upper, Lowerlist, Upperlist),
   atom_chars( Upper, Upperlist).

% prints out the trip in desired format
print_trip( Action, Code, Name, time( Hour, Minute)) :-
   to_upper( Code, Upper_code),
   format( "%-6s  %3s  %-16s  %02d:%02d",
           [Action, Upper_code, Name, Hour, Minute]),
   nl.

% prints the viable flight schedule out
printSched( [Dep|Tail] , Current) :-
   pop( Tail, Arr ),
   airport( Dep, DepName, _, _ ),
   hoursMins( Dep, Arr, HourD, MinsD ),
   DepMins is HourD * 60 + MinsD,
   DepMins >= Current,

   airport( Arr, ArrName, _, _ ),
   minutes( Dep, Arr, MinA ),
   MinA > Current,

   time( Dep, Arr , TripA ),
   ArrTime is MinA + TripA,
   TimeCurr is ArrTime + 30,
   convertTime( ArrTime, ArrHours, ArrMins ),
   print_trip( depart, Dep, DepName, time( HourD, MinsD)),
   print_trip( arrive, Arr, ArrName, time( ArrHours, ArrMins)),
   printSched( Tail, TimeCurr ).

printSched( [_], _ ).
