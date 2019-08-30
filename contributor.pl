:- module(contributor,
	[ married/2,			% +Person1, +Person2
	  medium_contributor/1,	% +Person1
	  meets_income/1		% +Person
	]).

/** <module> Exercise 2.
	
	Determine if a person is medium contributor or not. The following cases need to be covered:
		* Can not be a foreigner
		* If married, gross income of both parterns together can not exceed 70.000
		* Its private income can not exceed 40.000
	
	@author Pablo Martinez Perez
	@author David Montalvo Garcia
	@version 1.0

*/

native(javier_bardem).
native(javier_camara).
native(penelope_cruz).

foreigner(brad_pitt).
foreigner(angelina_jolie).

partner(javier_bardem, penelope_cruz).
partner(brad_pitt, angelina_jolie).

gross_income(javier_bardem, 3000).
gross_income(javier_camara, 20000).
gross_income(penelope_cruz, 55000).
gross_income(brad_pitt, 100000).
gross_income(angelina_jolie, 50000).


%!	married(+Person1:String, +Person2:String).
%
%	Checks if two people are married.
%
%	Examples:
%	==
%	?- married(brad_pitt, angelina_jolie).
%	true.
%	
%	?- married(javier_bardem, Y).
%	penelope_cruz.
%	==
%
%	@param Person1 First person to be checked if married.
%	@param Person2 Second person to be checked if married.
married(Person1, Person2) :- partner(Person1, Person2), ! .
married(Person1, Person2) :- partner(Person2, Person1).


%!	medium_contributor(+Person:String).
%
%	Checks if a person is a medium contributor. To be so, it can not be a foreigner and needs to be 
%	native. This double checking is done in case someone has double nationality, in which case it is
%	not a medium contributor.
%
%	@param Person Specific name for a person.
medium_contributor(Person) :- foreigner(Person), fail.
medium_contributor(Person) :- native(Person), meets_income(Person).


%!	meets_income(+Person:String).
%
%	Checks if a person meets the income requirements to be a medium contributor
%
%	@param Person Specific name for a person.
meets_income(Person) :- 
	married(Person, Y), 
	gross_income(Person, Income1), 
	gross_income(Y, Income2), 
	Income1 + Income2 > 70000, 
	!, 
	fail.
meets_income(Person) :- 
	gross_income(Person, Income), 
	Income =< 40000 .
