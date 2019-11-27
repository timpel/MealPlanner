/*
------- A Prolog ingredient Tracker and Recipe Book ---------
----------- By Tim Pellissier and Jordan Kroll --------------
*/

% Make procedures dynamic
:- dynamic have/1.
:- dynamic requires/2.

% Default Ingredients
have(mushrooms).
have(salt).
have(pepper).
have(cheese).

% Default Recipes
requires(omelette, [eggs, salt, pepper, cheese]).

% canmake item I with given list of ingredients.
canmake(_, []).
canmake(I, [H | R]) :- have(H), canmake(I, R).

% remove element E from L1 to get L2.  TODO: test this.
remove(E, [E | T], T) :- !.
remove(E, [H | T], [H | R]) :- remove(E, T, R).

% filter elements of L2 out of L1 to get L3. TODO: test this.
filter(L1, [], L1).
filter(L1, [H | T], L3) :- remove(H, L1, N1), filter(N1, T, L3).

% stillneed gives a list of ingredients still needed for item I.
stillneed(I, [H | T], [H | O]) :-
 \+ have(H),
 stillneed(I, T, O).

stillneed(I, [H | T], O) :-
  have(H),
  stillneed(I, T, O).

stillneed(_, [], []).

%%% Input.

% Adding an ingredient.
input(['add' | []]).

input(['add', H | T]) :-
  have(H),
  write("You already have: "), write(H), write(".\n"), flush_output(current_output),
  input(['add'| T]), !.

input(['add', H | T]) :-
  assert(have(H)),
  write("Added: "), write(H), write(".\n"), flush_output(current_output),
  input(['add'| T]), !.

input(['i', 'have' | T]) :-
  input(['add' | T]).

input(['i', 'bought', T]) :-
  input(['add' | T]).

% Querying for a single ingredient.
input(['do', 'i', 'have', X]) :-
  have(X),
  write("Yes you have "),
  write(X), write(".\n"), flush_output(current_output), !.

input(['do', 'i', 'have', X]) :-
  \+ have(X),
  write("No, you do not have "),
  write(X), write(".\n"), flush_output(current_output), !.

input(['do', 'i', 'have', D, X]) :-
  det(D), input(['do', 'i', 'have', X]).

% Get all ingredients.
input(['what', 'do', 'i', 'have' | _]) :-
  findall(X, have(X), List_of_ingredients),
  write("The current ingredients are: "),
  write(List_of_ingredients), write(".\n"), flush_output(current_output).

% Removing an ingredient.
input(['remove' | []]).

input(['remove', H | T]) :-
  have(H),
  retract(have(H)),
  write("Removed: "),
  write(H), write(".\n"), flush_output(current_output),
  input(['remove'| T]), !.

input(['remove', H | T]) :-
  \+ have(H),
  write("You didn't have item: "),
  write(H), write(".\n"), flush_output(current_output),
  input(['remove'| T]), !.

input(['i', 'used' | T]) :-
  input(['remove' | T]).

input(['i', 'do', 'not', 'have' | T]) :-
  input(['remove' | T]).

% Adding (or modifying) a recipe.
input([X, ':' | R]) :-
  remove_punc(R, T),
  requires(X,T),
  write("You already have this recipe for "),
  write(X), write(".\n"), flush_output(current_output), !.

input([X, ':' | R]) :-
  remove_punc(R, T),
  requires(X,_),
  retract(requires(X,_)),
  assert(requires(X,T)),
  write("The recipe for "),
  write(X), write(" has been updated.\n"), flush_output(current_output), !.

input([X, ':' | R]) :-
  remove_punc(R, T),
  assert(requires(X,T)),
  write("Recipe added for "),
  write(X), write(".\n"), flush_output(current_output), !.

% Querying for all recipes (whether they can be made or not).
input(['list', 'recipes']) :-
  findall((X,Y), requires(X,Y), Z),
  write("Your recipes are: "),
  write(Z), write(".\n"), flush_output(current_output).

input(['list', X, 'recipes']) :-
  det_for_plural(X),
  input(['list', 'recipes']).

input(['what', 'are', X, 'recipes']) :-
  any_det(X),
  input(['list', 'recipes']).

input(['what', 'are', X, Y, 'recipes']) :-
  det_for_plural(X),
  det(Y),
  input(['list', 'recipes']).


% Querying if a recipe can be made.
input(['can', 'i', 'make', X | _]) :-
  requires(X, Ings),
  stillneed(X, Ings, []),
  write("You have all the ingredients for "),
  write(X), write(".\n"), flush_output(current_output), !.

input(['can', 'i', 'make', X | _]) :-
  requires(X, Ings),
  stillneed(X, Ings, L),
  write("No, you are missing the following ingredients: "),
  write(L), write(".\n"), flush_output(current_output), !.

input(['can', 'i', 'make', Y, X | _]) :-
  det(Y),
  input(['can', 'i', 'make', X | _]).

% Querying for potential meals.
input(['what', 'can', 'i', 'make' | _]) :-
  findall(X, (requires(X,Y), canmake(X,Y)), Z),
  write("The items you can make are: "),
  write(Z), write(".\n"), flush_output(current_output).

% Politeness.
input(['please' | T]) :- input(T).

% Unrecognized input. Must be the last input case.
input(_) :-
  write("Input Not Recognized. Please try Another.\n"), flush_output(current_output).

% member(X,L) is true if X is an element of list L
  member(X,[X|_]).
  member(X,[_|R]) :- member(X, R).

% determiners
  det(X) :- member(X, ['a','an','the','one','some','any', 'my']).
  det_for_plural(X) :- member(X, ['some','all','my','the']).
  any_det(X) :- det(X), !.
  any_det(X) :- det_for_plural(X), !.

% punctuation
  punctuation(X) :- member(X, ['.',',','!','?']).

% remove all punctuation: O is list I with punctuation removed.
  remove_punc([], []).
  remove_punc([H | R], O) :- punctuation(H), !, remove_punc(R, O).
  remove_punc([H | R], [H | O]) :- remove_punc(R, O).

% checkquit loop
checkquit(['ex' | _]) :- !.
checkquit(['exit' | _]) :- !.
checkquit(L) :-
  input(L),
  q.

i :- q.
q :-
  write("\nAsk a query, update recipes, or update ingredients: "), flush_output(current_output),
  readln(L),     % L is an array of words
  maplist([I,O]>>(downcase_atom(I,O)), L, N),
  checkquit(N).
