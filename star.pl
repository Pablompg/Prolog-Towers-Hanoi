:- module(star,
	[ a_star_algorithm/4,		% +State, +Goal, -Path, -Num_explored_nodes
	  search/5,					% +Goal, +Frontier, +Explored_states, -Path, -New_explored_states
	  add_to_frontier/5,		% +Frontier, +Node, +Goal, +Explored_states, -New_ordered_frontier
	  expand/3,					% +Node, +Explored_states, -Childs
	  expand/4,					% +Node_state_tail, +Node, +Explored_states, -Childs
	  generate_child/3,			% +State_tail, +State, -Child_state
	  g_cost/3,					% +State1, +State2, -Cost
	  matching_point/3,			% +List1, +List2, -Index
	  evaluate_nodes/3,			% +Frontier, +Goal, -F_values_inversed
	  heuristic_value/3,		% +State, +Goal, -H_value
	  add_heuristic_option/1,	% +Option
	  delete_heuristic_option/1,% +Option
	  h2/3,						% +State, +Goal, -Num
	  h1/3,						% +State, +Goal, -Num
	  gap_to_goal/3,			% +State, +Goal, -Num
	  min_gap/4,				% +Element, +State, +Goal, -Min
	  position/3				% +State, +Element, -Index
	]).
:- use_module(library(lists)).
:- use_module(library(pairs)).

/** <module> A* Algorithm

	This module defines predicates to execute the A* (A-star) search algorithm.

	@author Pablo Martinez Perez
	@author David Montalvo Garcia
	@version 1.7
	
*/
:- dynamic heuristic/1.

%!	a_star_algorithm(+State:list, +Goal:list, -Path:list, -Num_explored_nodes:int).
%
%	Returns the optimum path from the State given to the Goal for the panckes relaxed problem, 
%	and the number of nodes explored in Num_explored_nodes.
%
%	Examples:
%	==
%	?- a_star_algorithm([3,1,4,2], [4,3,2,1], Path, Explored_nodes).
%	Path = [[3, 1, 4, 2], [3, 1, 2, 4], [4, 2, 1, 3], [4, 3, 1, 2], [4, 3, 2, 1]],
%	Explored_nodes = 15.
%
%	?- a_star_algorithm([2,5,3,1,4,6], [3,4,6,2,1,5], Path).
%	Path = [[2, 5, 3, 1, 4, 6], [2, 5, 3, 1, 6, 4], [2, 5, 3, 4, 6, 1], [2, 5, 1, 6, 4, 3], 
%		[3, 4, 6, 1, 5, 2], [3, 4, 6, 2, 5, 1], [3, 4, 6, 2, 1, 5]],
%	Explored_nodes = 239.
%	==
%
%	@param State			  Current state.
%	@param Goal				  Goal state.
%	@param Path				  Path found (which is optimum due to A*).
%	@param Num_explored_nodes Number of nodes explored.
a_star_algorithm(State, Goal, Path, Num_explored_nodes) :-
	search(Goal, [[State, [State], 0]], [], Path, Num_explored_nodes).
	
%!	search(+Goal:list, +Frontier:list, +Explored_states:list, -Path:list, -New_explored_states:int).
%
%	Returns the solution path and the number of nodes explored. The first node of the frontier is 
%	extracted and compared to the Goal node. The following cases are possible:
%		* If the extracted node matches the target node, the path associated with the node is
%		returned.
%		*  Otherwise, the current node is expanded and added to the border using the predicate
%		add_to_frontier/5. The current node is added to the list of explored nodes and search/4 is
%		called again with the new ordered border and the new list of expanded nodes.
%
%	If the frontier is empty, the solution is not found and the empty path is returned.
%
%	The nodes are represented by lists of three elements:
%	==
%	Node = [Node_state, Node_path, G_value]
%	==
%	where Node_path is the current path to the node defined as followed:
%	==
%	Node_path = [[State1], [State2], [State3], ..., [StateN]]
%	==
%
%	@param Goal  			  List of the goal state.
%	@param Frontier  		  List of frontier nodes.
%	@param Explored_states 	  List of explored states.
%	@param Path  			  List of founded path.
%	@param Num_explored_nodes Number of nodes explored.
search(_, [], _, [], _).
search(Goal, [Node|_], _, Path, 1) :-
	Node = [Node_state, Node_path, _],
	Goal == Node_state,
	!,
	Path = Node_path.
search(Goal, [Node|Frontier], Explored_states, Path, Num_explored_nodes) :-
	Node = [Node_state, _, _],
	add_to_frontier(Frontier, Node, Goal, Explored_states, New_ordered_frontier),
	append(Explored_states, [Node_state], New_explored_states),
	search(Goal, New_ordered_frontier, New_explored_states, Path, Previous_num_explored_nodes), 
	Num_explored_nodes is Previous_num_explored_nodes + 1.

%!	add_to_frontier(+Frontier:list, +Node:list, +Goal:list, +Explored_states:list, -New_ordered_frontier:list).
%
%	Adds the child nodes of Node to Frontier if they have not been previously explored, and 
%	returns this new frontier in the variable New_ordered_frontier. To do so, the following steps 
%	are carried out:
%		1. The list of child nodes of Node is obtained using expand/3.
%		2. New nodes are added to the border.
%		3. A list (called Frontier_values_inversed) is generated with the values of the 
%		evaluation function applied to each node of the frontier, but inverted. This inverted 
%		list is inverted again to obtain the f-values in the right order, and stored in 
%		Frontier_values.
%		4. Using the predicate pairs_keys_values/3 (available in library pairs) each node of 
%		the border is associated to its f-function value. This set of key-value pairs is stored 
%		in the variable Pairs_key_value.
%		5. Finally, the list of keys-value Pairs_key_value is sorted using keysort/2 predicate, 
%		and the f-function value is removed from the list using pairs_values/2. The list of 
%		already ordered nodes is stored in New_ordered_frontier.
%
%	@param Frontier 			List of nodes to be explored, sorted by evaluation function.
%	@param Node					Current node which is being evaluated and might be added to 
%								New_ordered_frontier if it has not already been explored.
%	@param Goal					Goal state.
%	@param Explored_states		List where all explored states are stored.
%	@param New_ordered_frontier Frontier which contains the new child nodes in order (depending 
%	on g()).
add_to_frontier(Frontier, Node, Goal, Explored_states, New_ordered_frontier) :-
	expand(Node, Explored_states, Child_nodes),
	append(Frontier, Child_nodes, New_frontier),
	evaluate_nodes(New_frontier, Goal, Frontier_values_inversed),
	reverse(Frontier_values_inversed, Frontier_values),
	pairs_keys_values(Pairs_key_value, Frontier_values, New_frontier),
	keysort(Pairs_key_value, Ordered_mapped_frontier),
	pairs_values(Ordered_mapped_frontier, New_ordered_frontier).

%!	expand(+Node:list, +Explored_states:list, -Childs:list).
%
%	Returns a list with the child nodes of Node that have not yet been explored in the variable 
%	Childs. To do this, expand/4 is used.
%	
%	@param Node 		   Current node which is being expanded.
%	@param Explored_states List where all explored states are stored.
%	@param Childs		   Child nodes of Node that have not yet been explored.
expand(Node, Explored_states, Childs) :-
	Node = [Node_state, _, _],
	expand(Node_state, Node, Explored_states, Childs).

%!	expand(+Node_state_tail:list, +Node:list, +Explored_states:list, -Childs:list).
%
%	Creates a list of nodes with the child nodes of Node that have not yet been explored, using 
%	for this purpose the Explored_states variable. Recursion is used to cover all possible child 
%	states of Node_state, by decreasing the size of the Node_state_tail with each recursive call.
%
%	For each recursive step, the child state is generated by the predicade generate_child/3, and 
%	it is checked if the generated state is alredy in the list Explored_states, using member/2. 
%	Two cases may occur:
%		1. The node has already been explored. In this case the node is not generated.
%		2. The node hasn't been explored yet. In this case, the cost of switching from the 
%		current state to the child state is calculated using g_cost/3, and this cost is added to 
%		the accumulated cost of Node. Finally:
%			* The child state is added to the path of the parent node.
%			* The child node is created as a list composed of child state, the previously 
%			calculated path and the accumulated cost of the node.
%			* This new node is added to the returned Childs list.
%
%	Example:
%	==
%	?- expand([3,1,2,4], [[3,1,2,4],[[3,1,4,2],[3,1,2,4]],2], [[3,1,4,2],[3,1,2,4]], Childs).
%	Childs = [[[3, 4, 2, 1], [[3, 1, 4, 2], [3, 1, 2, 4], [3, 4, 2, 1]], 5], 
%		  [[4, 2, 1, 3], [[3, 1, 4, 2], [3, 1, 2, 4], [4, 2, 1, 3]], 6]].
%	==
%
%	@param Node_state_tail	Tail of the given state which decreases with each recursive call.
%	@param Node				Current node which is being expanded.
%	@param Explored_states	List where all explored states are stored.
%	@param Childs			Child nodes of Node that have not yet been explored.
expand([_|[]], _, _, []) :-
	!.
expand([X|Xs], Node, Explored_states, Childs_list) :-
	Node = [Node_state, _, _],
	generate_child([X|Xs], Node_state, Child_state),
	member(Child_state, Explored_states),
	!,
	expand(Xs, Node, Explored_states, Childs_list).
expand([X|Xs], Node, Explored_states, Childs_list) :-
	Node = [Node_state, Node_path, G_value],
	expand(Xs, Node, Explored_states, Childs),
	generate_child([X|Xs], Node_state, Child_state),
	g_cost(Node_state, Child_state, Added_cost),
	New_g_value is Added_cost + G_value,
	append(Node_path, [Child_state], New_node_path),
	append(Childs, [[Child_state, New_node_path, New_g_value]], Childs_list).

%!	generate_child(+State_tail:list, +State:list, -Child_state).
%
%	Creates a child of a State using a tail from it. It substracts the tail from State and 
%	afterwards adds the reversed tail to State.
%
%	Examples:
%	==
%	?- generate_child([1,2,3,4], [1,2,3,4], Child).
%	Child = [4 ,3, 2, 1].
%
%	?- generate_child([3,4,5], [1,2,3,4], Child).
%	false.
%	==
%
%	@param State_tail  Tail of the given state used to generate the child.
%	@param State	   The state from whom the child is generated.
%	@param Child_state Child state generated.
generate_child(Tail, State, Child_state) :-
	is_set(Tail),
	subset(Tail, State),
	subtract(State, Tail, Head),
	reverse(Tail, Inverted_tail),
	append(Head, Inverted_tail, Child_state).

%!	g_cost(+State1:list, +State2:list, -Cost:list).
%
%	Determines the cost of moving from one state to another for the relaxed pancake problem (how 
%	many pancakes are raised to make a movement). 
%	// TODO: hacer una función is_child de tal forma que si no es hijo, te devuelva false.
%
%	Examples:
%	==
%	?- g_cost([1,2,3,4,5], [1,2,5,4,3], Cost).
%	Cost = 3.
%
%	?- g_cost([1,2,3,4,5], [1,2,3,4,5], Cost).
%	Cost = 0.
%
%	?- g_cost([1,2,3,4,5], [5,2,3,4,1], Cost).
%	Cost = 5.
%	==
%
%	@param State1 First list given with the pancakes position.
%	@param State2 Second list given with the pancakes position.
%	@param Cost	  Cost of moving from State1 to State2.
g_cost(State1, State2, Cost) :-
	matching_point(State1, State2, Index),
	length(State1, Length),
	Cost is Length - Index.
	
%!	matching_point(+List1:list, +List2:list, -Index:list).
%
%	Determines up to which index two lists exactly match.
%
%	Examples:
%	==
%	?- matching_point([1,2,3,4], [1,2,4,3], Index).
%	Index = 0+1+1.
%	
%	?- matching_point([1,2,3,4], [1,2,3,4], Index).
%	Index = 0+1+1+1+1.
%
%	?- matching_point([4,2,3,1], [4,1,3,2], Index).
%	Index = 0+1.
%	==
%
%	@param List1 First list given that is to be compared.
%	@param List2 First list given that is to be compared.
%	@param Index Index determing until which position List1 and List2 exactly match.
matching_point([], _, 0) :-
	!.
matching_point([X|_], [Y|_], 0) :-
	X \= Y,
	!.
matching_point([_|Xs], [_|Ys], Cost) :-
	matching_point(Xs, Ys, Num),
	Cost = Num + 1.
	
%!	evaluate_nodes(+Frontier:list, +Goal:list, -F_values_inversed:list).
%	
%	For each Node in the frontier, it calculates its heurstic value using h2/3 recursively. It 
%	then returns a list of integers containg the f (g + h) value for each node. This list is in 
%	reversed order.
%
%	@param Frontier		   	 The frontier containing the nodes for which the f value are to be 
%							 evaluated.
%	@param Goal	   			 Goal node.
%	@param F_values_inversed Reversed list containing the f values for each node in Frontier.
evaluate_nodes([], _, []).
evaluate_nodes([Node|Xs], Goal, Frontier_values) :-
	evaluate_nodes(Xs, Goal, Frontier_aux),
	Node = [Node_state, _, G_value],
	heuristic_value(Node_state, Goal, H_value),
	Num is H_value + G_value,
	append(Frontier_aux, [Num], Frontier_values).

%!	heuristic_value(+State:list, +Goal:list, -H_value:int)
%	
%	Computes the heursitic value given for two states given. There are three possible heurisitcs:
%		1. 0 regardless of the two states provided if heurisitc(0) is true.
%		2. h1 if heuristic(1) is true.
%		3. h2 if heuristic(2) is true.
%
%	@param State   State provided.
%	@param Goal	   Goal node.
%	@param H_value Heuristic value computed for the two states given.
heuristic_value(State, Goal, H_value) :-
	heuristic(2),
	!,
	h2(State, Goal, H_value).
heuristic_value(State, Goal, H_value) :-
	heuristic(1),
	!,
	h1(State, Goal, H_value).
heuristic_value(_, _, 0).

%!	add_heuristic_option(+Option:int).
%
%	Add to the knowledge base the following fact: heuristic(Option).
%
%	@param Option Number of the heuristic to be added
add_heuristic_option(Option) :-
	assert(heuristic(Option)).

%!	delete_heuristic_option(+Option:int).
%
%	Delete to the knowledge base the following fact: heuristic(Option).
%
%	@param Option Number of the heuristic to be deleted	
delete_heuristic_option(Option) :-
	retract(heuristic(Option)).
	
%!	h2(+State:list, +Goal:list, -Num:int).
%
%	Calculates the heuristic value of the current state as the sum of the distances between 
%	the current position of each pancake and its current position, minus the number of misplaced
%	pancakes.
%
%	@param State Current state.
%	@param Goal  Goal state.
%	@param Num   The heuristic value, which corresponds to the number of pancakes poorly placed.
h2(State, Goal, Num) :-
	h1(State, Goal, Num2),
	gap_to_goal(State, Goal, Num1),
	Num is Num1 - Num2.

%!	h1(+State:list, +Goal:list, -Num:int).
%
%	Calculates the heuristic value of the current state as the number of misplaced pancakes.
%
%	@param State Current state.
%	@param Goal  Goal state.
%	@param Num   The heuristic value, which corresponds to the number of pancakes poorly placed.
h1([], [], 0).
h1([X|Xs], [X|Ys], Num) :-
	h1(Xs, Ys, Num), !.
h1([_|Xs], [_|Ys], Num) :-
	h1(Xs, Ys, N), Num is N + 1.

%!	gap_to_goal(+State:list, +Goal:list, -Num:int).
%
%	For a given state it determines the heuristic cost of moving to Goal. To do so, it recursively 
%	determines the cost for each Element in State and sums them all using min_gap/4.
%
%	@param State Current state.
%	@param Goal  Goal state.
%	@param Num   Heuristic value of moving from State to Goal.
gap_to_goal(State, Goal, Num) :-
	gap_to_goal(State, State, Goal, Num).
gap_to_goal([], _, _, 0).
gap_to_goal([X|Xs], State, Goal, Num) :-
	gap_to_goal(Xs, State, Goal, Min),
	min_gap(X, State, Goal, Min_aux),
	Num is Min + Min_aux.

%!	min_gap(+Element:Generic, +State:list, +Goal:list, -Min:int).
%
%	Calculates the minimun gap between the position of an element in the State list and its 
%	position in Goal list. These steps are followed:
%		1. The index in which Element is in the State is saved in State_index.
%		2. The index in which Element is in the Goal is saved in Goal_index.
%		3. The absolute value of the difference between State_index and Goal_index is calculated.
%		This value is saved in Min1.
%		4. The lenght of Goal is saved in List_length (Which is the same for State and Goal).
%	 	5. The difference between List_length and Min1 is calculated. This value is denoted
%		by Min2. Min2 denotes the cyclic distance.
%	 	6. Take as a Min the minimum between Min1 and Min2. The minimun value between distance and
%		the ciclical distance is returned.
%
%	@param Element An element (should be in both state and Goal).
%	@param State   Current state.
%	@param Goal    Goal state.
%	@param Min     The minimun gap between the position of an element in the State list and its 
%				   position in Goal list. That is, the number of movements needed to move an Element
%				   from State to Goal allowing cyclical movements.
min_gap(Element, State, Goal, Min) :-
	position(State, Element, State_index), 
	position(Goal, Element, Goal_index), 
	Min1 = abs(State_index - Goal_index),
	length(Goal, List_length),
	Min2 is List_length - Min1,
	Min is min(Min1, Min2).

%!	position(+State:list, +Element:list, -Index:int).
%
%	Returns the position (index) of the first appearance of an element in a list or false if the
%	element is not found. It differs from nth0 as it only returns the first appearance.
%
%	@param State 	List in which the element will be searched for.
%	@param Element  Element to be search for.
%	@param Index    Position of the State in the list (Index of element in list)
position([], _, _) :-
	fail.
position([X|_], X, Index) :-
	!,
	Index = 0.
position([_|Xs], E, Index) :-
	position(Xs, E, N),
	Index is N + 1.