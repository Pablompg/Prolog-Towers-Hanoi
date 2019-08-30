# Prolog Algorithms. A*

This is my intelligent systems final assignment. The problem being solved is similar to the _Towers of Hanoi_ problem, for which the A* search algorithm is implemented allowing the user to chose between different heuristics to find the optimum path. As always when using prolog, a recursive aproach is used.


## Problem

> Imagine we have a series of Pancakes of different sizes stacked one on top of the other. Lets assume we have *'n'* panckes and all of them differ in size. For a given start state find the optimum path to the desired final state which also needs to be given. 
The pancackes can only be moved as if a spatula was being used, as in the following image.
<p align="center">
  <img src="PancackesImage.png" alt="" width="700">
</p>

## Approach
The A* search algorithm has being implemented using two different heuristics. The menu gives the user the option of choosing which one to use.

## Installation and user Instructions
1. Dowload and install prolog at https://www.swi-prolog.org/Download.html
2. Download this project
3. Open prolog and load/consult `menu.pl`
4. Execute `search.` in the command line. A menu will appear. Follow its instructions.

## Documentation Instructions
There are two methods to consult the documentation for this project
1. A pdf can be found in this very same directory with all the necessary documentation explained
2. Open prolog and load/consult `documentation.pl` and wait for a few seconds. This prolog script starts the documentation server at port 4000 and opens the user’s default browser on the running documentation server.
    * Select the local directory from which you want to see the documentation (upper left options bar).
    * Select the file you want to read (A-star is the most important file where the main's algorithms are written and explained.
    * If you want to see the source code, you can click the  orange icon on the right side of each predicate (or the orange icon at the top).

## License
This project is under  [MIT LICENCE](LICENSE). 

## Authors
This project was developed by Pablo Martínez Pérez and [David Montalvo García](https://github.com/davidMontalvoGarcia)

### Remark
This project was done for academic purposes and its implementation is only intended for educational purposes.