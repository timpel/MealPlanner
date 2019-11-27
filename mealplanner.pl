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
input(['i', 'have', X | _]) :-
  have(X), !.
input(['i', 'have', X | _]) :-
  assert(have(X)).
input(['i', 'bought', X | _]) :-
  have(X), !.
input(['i', 'bought', X | _]) :-
  assert(have(X)).

% Get all ingredients.
input(['what', 'do', 'i', 'have' | _]) :-
  findall(X, have(X), List_of_ingredients),
  write("The current ingredients are: "),
  write(List_of_ingredients), flush_output(current_output).

% Removing an ingredient.
input(['i', 'used', X | _]) :-
  have(X), retract(have(X)),
  write("Removed: "), write(X), flush_output(current_output), !.
input(['i', 'used', X | _]) :-
  write("You didn't have item: "), write(X), flush_output(current_output).
input(['i', 'do', 'not', 'have', X | _]) :-
  have(X), retract(have(X)),
  write("Removed: "), write(X), flush_output(current_output), !.
input(['i', 'do', 'not', 'have', X | _]) :-
  write("You didn't have item: "), write(X), flush_output(current_output).

% Adding a recipe.
input([X, ':' | R]) :-
  remove_punc(R, T),
  requires(X,T), !.
input([X, ':' | R]) :-
  remove_punc(R, T),
  assert(requires(X,T)).

% Querying for all recipes (whether they can be made or not).
input(['list', 'recipes' | _]) :-
  findall((X,Y), requires(X,Y), Z),
  write("Your recipes are: "),
  write(Z), flush_output(current_output).

input(['list', X, 'recipes' | _]) :-
  det_for_plural(X),
  input(['list', 'recipes' | _]).

% Querying if a recipe can be made.
input(['can', 'i', 'make', X | _]) :-
  requires(X, Ings),
  stillneed(X, Ings, []),
  write("You have all the ingredients for "),
  write(X), flush_output(current_output), !.

input(['can', 'i', 'make', X | _]) :-
  requires(X, Ings),
  stillneed(X, Ings, L),
  write("No, you are missing the following ingredients: "),
  write(L), flush_output(current_output), !.

input(['can', 'i', 'make', Y, X | _]) :-
  det(Y),
  input(['can', 'i', 'make', X | _]).

% Querying for potential meals.
input(['what', 'can', 'i', 'make' | _]) :-
  findall(X, (requires(X,Y), canmake(X,Y)), Z),
  write("The items you can make are: "),
  write(Z), flush_output(current_output).

% Politeness.
input(['please' | T]) :- input(T).

% Unrecognized input. Must be the last input case.
input(_) :-
  write("Input Not Recognized. Please try Another."), flush_output(current_output).

% member(X,L) is true if X is an element of list L
  member(X,[X|_]).
  member(X,[_|R]) :- member(X, R).

% determiners
  det(X) :- member(X, ['a','an','the','one','some']).
  det_for_plural(X) :- member(X, ['some','all','my']).

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
