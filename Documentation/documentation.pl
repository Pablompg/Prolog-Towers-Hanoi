:- module(documentation,[]).

/** <module> See online documentation.

	This Prolog script starts documentation server at port 4000 and opens the user's default browser 
	on the running documentation server. To see the code, click on the orange icon on the right side
	of each predicate.

	@author Pablo Martinez Perez
	@author David Montalvo Garcia
	@version 1.0
*/

:- doc_server(4000).
:- doc_browser.
:- [numbers,
	contributor,
	menu,
	star
	].
	