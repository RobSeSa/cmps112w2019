% $Id: factorial.pl,v 1.3 2019-03-06 15:38:56-08 - - $ */

%
% Factorial, the old intro to recursion standby.
%

factorial( 0, 1 ).

factorial( N, Nfac ) :-
	M is N - 1,
	factorial( M, Mfac ),
	Nfac is N * Mfac.

% TEST: factorial(5,N).
% TEST: factorial(20,N).
