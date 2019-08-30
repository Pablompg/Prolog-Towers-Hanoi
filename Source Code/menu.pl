:- module(menu,
	[ search/0,
	  menu/1,				% +Num
	  execute_algorithm/1,	% +Option
	  read_states/2,		% -Start_state, -Goal
	  cls/0
	]).

:- use_module(star).

/** <module> Exercise 3.

	Shows a menu to allow the user to execute the A* Algorithm or Lowest Cost First Algorithm.

	@author Pablo Martinez Perez
	@author David Montalvo Garcia
	@version 1.7
	
*/

%!	search.
%
%	The user is shown a menu to choose between 2 search algorithms. There are 3 possiblities:
%		0. Exit: menu(0) is called and the program finishes.
%		1. Lowest Cost First: menu(1) is called and the user is asked for more information.
%		2. A*: menu(2) is called, another menu is shown and the user is asked for more information.
search :- 
	repeat, cls,
    write('      -- MENU --      '), nl, nl,
    write('  1. Lowest cost First'), nl,
    write('  2. A*               '), nl,
	write('  0. Exit             '), nl, nl,
    write('  Enter your choice: '),
    read(Choice), Choice >= 0, Choice =< 2,
    menu(Choice), Choice = 0, !.
	
%!	menu(+Num:int).
%	
%	Menu that asks the user to introduce input in order to get the start state and the goal state.
%
%	@param Num Index that determines which heuristic will be used to compute the path.
menu(0).
menu(1) :- 
	cls,
	write('  --  Lowest Cost First Algorithm -- '), nl,
	execute_algorithm(0).
menu(2) :- 
	repeat, cls,
    write('      -- A* --     '), nl, nl,
    write('  1. Heuristic 1    '), nl,
    write('  2. Heuristic 2    '), nl,
	write('  0. Return         '), nl, nl,
    write('  Enter your choice'),
    read(Choice), Choice >= 0, Choice =< 2,
    !, Choice \= 0, Option is Choice + 2, menu(Option).
menu(3) :- 
	cls,
	write('  --  A* Heuristic 1 -- '), nl,
	execute_algorithm(1).
menu(4) :- 
	cls,
	write('  --  A* Heuristic 2 -- '), nl,
	execute_algorithm(2).

%!	execute_algorithm(+Option:int)
%
%	This predicate calls read_states/2, and adds to the knowledge base the heuristic value. Then 
%	executes the a_star_algorithm/4, displays the results and removes from knowledge base the 
%	stored heuristic value.
%
%	@param Option Index determing which heuristic is added to the knowledge base
execute_algorithm(Option) :-
	read_states(State, Goal), 
	add_heuristic_option(Option),
	a_star_algorithm(State, Goal, Path, Num_explored_nodes),
	delete_heuristic_option(Option),
	write('  Path: '), write(Path), nl,
	write('  Explored nodes: '), write(Num_explored_nodes), nl,
	write('\nEnter any char to continue'), read(_).

%!	read_states(-Start_state:list, -Goal:list)
%
%	Asks the user to introduce a specific State and a Goal state. Both need to be lists. Aftweards 
%	it checks both states are valid (both lists need to have the same elements).
%
%	@param Start_state Start State that the user is ask to provide.
%	@param Goal  	   Goal state that the user is ask to provide.
read_states(Start_state, Goal) :- 
	repeat,
	write('Introduce your start State (as a list)'), read(Start_state), nl,
	write('Introduce your goal node (as a list)'), read(Goal), nl,
	subset(Start_state, Goal), subset(Goal, Start_state), !.

%!	cls
%
%	Cleans the screen.
cls :- 
	write('\e[2J'), nl.