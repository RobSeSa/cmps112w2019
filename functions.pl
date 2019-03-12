% Robert Sato (rssato@ucsc.edu)
% Huanlei Wu (hwu43@ucsc.edu)

not( X ) :- X, !, fail.
not( _ ).


fly( From, To ) :-
   isFlight( From, To, [From] ).

%if flight( From -> To ) true
isFlight( From, To , Tried ) :-
   write('Checking for direct flight from '), 
   write( From ), write(' to '), write( To ), nl,
   flight( From, To, _ ),
   append( Tried, [To], Path ), 
   printSched( Path , 0).

%elseif flight( From -> Temp1 -> To ) true
isFlight( From, To, Tried) :-
   nl, write('Start testing for From -> temp1 -> To.. '), nl,
   flight( From, Temp1, _),
   write('From = '), write(From), 
   write('; Temp1 = '), write(Temp1),
   write('; To = '), write(To), 
   write('; Tried = '), write(Tried), nl,
   not( member( Temp1, Tried ) ),
   write('flight temp1 temp2..'), nl,
   flight( Temp1, Temp2, _),
   write('From = '), write(From), 
   write('; Temp1 = '), write(Temp1),
   write('; Temp2 = '), write(Temp2),
   write('; To = '), write(To), 
   write('; Tried = '), write(Tried), nl,
   write('Checking if '), write(Temp2),
   write(' not a member of tried.. '),nl,
   not( member( Temp2, Tried ) ),
   write('Checking the time..'), nl,
   checkTime( From, Temp1, Temp2 ),
   write('Comparing Temp2 and To'), nl,
   Temp2 = To,
   write('Flight exists with one layover.. '), nl,
   write('append1.. '), nl,
   append( Tried, [Temp1], Path ), 
   write('append2.. '), nl,
   append( Path, [To], Final ), 
   write('printSched.. '), nl,
   printSched( Final , 0).

%else
isFlight( From, To, Tried) :-
   nl, write('Start testing in else clause.. '), nl,
   flight( From, Temp1, _),
   write('From = '), write(From), 
   write('; Temp1 = '), write(Temp1),
   write('; To = '), write(To), 
   write('; Tried = '), write(Tried), nl,
   not( member( Temp1, Tried ) ),
   flight( Temp1, Temp2, _),
   write('From = '), write(From), 
   write('; Temp1 = '), write(Temp1),
   write('; Temp2 = '), write(Temp2),
   write('; To = '), write(To), 
   write('; Tried = '), write(Tried), nl,
   not( member( Temp2, Tried ) ),
   write('Checking the time..'), nl,
   checkTime( From, Temp1, Temp2 ),
   append( Tried, [Temp1], Path ), 
   append( Path, [Temp2], Final ), 
   write('Adding two layovers..'), nl, nl,
   isFlight( Temp2, To, Final ).

%checks if the flight From to Layover makes it before Layover to To
checkTime( From, Layover, To ) :-
   minutes( From, Layover, MinA ),
   time( From, Layover, TripA ),
   minutes( Layover, To, MinB ),
   AToB is MinA + TripA + 30,
   write('Comparing flight '), write(Layover),
   write(' to '), write(To), 
   write(' departure time: '), write(MinB),
   write(' with '), write(From),
   write(' to '), write(Layover), 
   write(' departure + flight + 30: '), write(AToB), nl,
   MinB >= AToB.

%grabs time converted to minutes
minutes( From, To, Minutes ) :-
   flight( From, To, time( Hour, Min ) ),
   Minutes is Hour * 60 + Min.

%grabs hours and minutes
hoursMins( From, To, Hours, Minutes ) :-
   flight( From, To, time( Hour, Min ) ),
   Hours is Hour,
   Minutes is Min.

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

convertTime( Minutes, HoursO, MinsO ) :-
   HoursO is Minutes div 60,
   MinsO is Minutes mod 60.

to_upper( Lower, Upper) :-
   atom_chars( Lower, Lowerlist),
   maplist( lower_upper, Lowerlist, Upperlist),
   atom_chars( Upper, Upperlist).

print_trip( Action, Code, Name, time( Hour, Minute)) :-
   to_upper( Code, Upper_code),
   format( "%-6s  %3s  %-16s  %02d:%02d",
           [Action, Upper_code, Name, Hour, Minute]),
   nl.

pop( [Head|_], Value ) :-
   Value = Head.

addTime(Hours, Minutes, Total) :-
   Total is Hours * 60 + Minutes.


printSched( [Head], _ ) :-
   write(Head), nl.

printSched( [Dep|Tail] , Current) :-
   pop( Tail, Arr ),
   airport( Dep, DepName, _, _ ),
   hoursMins( Dep, Arr, HourD, MinsD ),
   addTime(HourD, MinsD, DepMins),
   Current =< DepMins,

   airport( Arr, ArrName, _, _ ),
   minutes( Dep, Arr, MinA ),
   Current =< MinA,

   time( Dep, Arr , TripA ),
   ArrTime is MinA + TripA,
   TimeCurr is ArrTime + 30,
   convertTime( ArrTime, ArrHours, ArrMins ),
   print_trip( depart, Dep, DepName, time( HourD, MinsD)),
   print_trip( arrive, Arr, ArrName, time( ArrHours, ArrMins)),
   printSched( Tail, TimeCurr ).


/*
test :-
   print_trip( depart, nyc, 'New York City', time( 9, 3)),
   print_trip( arrive, lax, 'Los Angeles', time( 14, 22)).

printSched( [Head] ) :-
   write( Head ), nl.

printSched( [Head|Tail] ) :-
   write( Head ), write(' -> '),
   printSched( Tail ).
*/
