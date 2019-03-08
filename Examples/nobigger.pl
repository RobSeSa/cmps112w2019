% $Id: nobigger.pl,v 1.4 2019-03-07 14:21:11-08 - - $ */

%
% Sample of the use of tracing.
% Find the biggest number in a list.
%

mynumber( 3 ).
mynumber( 6 ).
mynumber( 9 ).

biggest( Number ) :- mynumber( Number ), nobigger( Number ).

nobigger( Number ) :- mynumber( Other ), Other > Number, !, fail.

nobigger( _ ).

traceon :-
   trace( mynumber ),
   trace( biggest ),
   trace( nobigger ).

% TEST: biggest(N)..
