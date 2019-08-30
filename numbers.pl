:- module(numbers,
	[ is_number/1	% +List.
	]).

/** <module> Exercise 1.
	
	Determine if a list contains a number in its elements.
	
	@author Pablo Martinez Perez
	@author David Montalvo Garcia
	@version 1.0
	
*/

%!	is_number(+List:list).
%
%	Checks if the list given has a number.
%
%	Examples:
%	==
%	?- is_number([a, b, 1, c, d]).
%	true.
%	
%	?- is_number([a, b, c, d]).
%	false.
%	==
%
%	@param List List provided to look for numbers inside it.
is_number([]) :- 
	!,
	fail.
is_number([X|_]) :- 
	number(X),
	!.
is_number([_|Xs]) :- 
	is_number(Xs) .