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
input(['ingredients']) :-
  findall(X, have(X), List_of_ingredients),
  write("The current ingredients are: "),
  write(List_of_ingredients), write(".\n"), flush_output(current_output).

input(['what', 'do', 'i', 'have' | _]) :-
  input(['ingredients']).

input(['list','ingredients']) :-
  input(['ingredients']).

input(['list', D, 'ingredients']) :-
  det_for_plural(D),
  input(['ingredients']).

input(['get','ingredients']) :-
  input(['ingredients']).

input(['get', D, 'ingredients']) :-
  det_for_plural(D),
  input(['ingredients']).

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
  requires(X,R),
  write("You already have this recipe for "),
  write(X), write(".\n"), flush_output(current_output), !.

input([X, ':' | R]) :-
  requires(X,_),
  retract(requires(X,_)),
  assert(requires(X,R)),
  write("The recipe for "),
  write(X), write(" has been updated.\n"), flush_output(current_output), !.

input([X, ':' | R]) :-
  assert(requires(X,R)),
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

input(['list', X, Y, 'recipes']) :-
  det_for_plural(X),
  det(Y),
  input(['list', 'recipes']).

input(['what', 'are', X, 'recipes']) :-
  any_det(X),
  input(['list', 'recipes']).

input(['what', 'are', X, Y, 'recipes']) :-
  input(['list', X, Y, 'recipes']).

input(['get', 'recipes']) :-
  input(['list', 'recipes']).

input(['get', X, 'recipes']) :-
  input(['list', X, 'recipes']).

input(['get', X, Y, 'recipes']) :-
  input(['list', X, Y, 'recipes']).


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

% Saving to file.
input(['save']) :-
  save.

input(['save','store']) :- save.

% Loading a file.
input(['load']) :-
  [store],
  write("Ingredients and recipes have been successfully loaded.\n"), flush_output(current_output).

input(['load','store']) :- input(['load']).

% Asking for info
input(['h']) :- info.
input(['help']) :- info.
input(['info']) :- info.
input(['what','can','i','ask']) :- info.
input(['what','can','i','do']) :- info.
input(['list','queries']) :- list_queries.
input(['list','commands']) :- list_commands.

% Politeness.
input(['please' | T]) :- input(T).

% Unrecognized input. Must be the last input case.
input(_) :-
  write("Input Not Recognized. Please try Another.\n"), flush_output(current_output).

%%% Helper Functions.

% member(X,L) is true if X is an element of list L
  member(X,[X|_]).
  member(X,[_|R]) :- member(X, R).

% determiners
  det(X) :- member(X, ['a','an','the','one','some','any', 'my']).
  det_for_plural(X) :- member(X, ['some','all','my','the']).
  any_det(X) :- det(X), !.
  any_det(X) :- det_for_plural(X), !.

% punctuation
  punctuation(X) :- member(X, ['.',',','!','?']). % Does not include ':'

% remove all punctuation: O is list I with punctuation removed.
  remove_punc([], []).
  remove_punc([H | R], O) :- punctuation(H), !, remove_punc(R, O).
  remove_punc([H | R], [H | O]) :- remove_punc(R, O).

%%% Writing to file.
save :-
  open('store.pl',write,Stream),
  write(Stream, ":- dynamic have/1.\n"),
  write(Stream, ":- dynamic requires/2.\n"),
  findall(X, have(X), Ingredients),
  saveItems(Stream, Ingredients),
  findall(Y, requires(Y, _), Recipes),
  saveRecipes(Stream, Recipes),
  close(Stream),
  write("Ingredients and recipes have been successfully saved.\n"), flush_output(current_output).

saveItems(_, []).
saveItems(Stream, [H | T]) :-
  write(Stream, "have("),
  write(Stream, H),
  write(Stream, ").\n"),
  saveItems(Stream, T).

saveRecipes(_, []).
saveRecipes(Stream, [H | T]) :-
  write(Stream, "requires("),
  write(Stream, H),
  write(Stream, ", "),
  requires(H, L),
  write(Stream, L),
  write(Stream, ").\n"),
  saveRecipes(Stream, T).

%%% Info

info :-
  list_commands,
  list_queries,
  write("\nNote that there are variants of the above commands and queries. All of them are case insensitive. All punctuation other than ':' is ignored.\n"),
  flush_output(current_output).

list_commands :-
  write("Commands:\n"),
  write("  To add ingredients: add <ingredient> ...\n"),
  write("  To remove ingredients: remove <ingredient> ...\n"),
  write("  To add a recipe: <recipe>: <ingredient> ...\n"),
  write("      To modify a recipe: <recipe>: <ingredient> ...\n"),
  write("  To load stored ingredients and recipes (from file): load\n"),
  write("  To store ingredients and recipes (save to file): save\n"),
  flush_output(current_output).

list_queries :-
  write("Queries:\n"),
  write("  To get a list of all ingredients: What do I have?\n"),
  write("      Alternate query: list ingredients\n"),
  write("  To query for a particular ingredient: Do I have <ingredient>?\n"),
  write("  To get a list of all recipes: What are my recipes?\n"),
  write("      Alternate query: list recipes\n"),
  write("  To query for whether a recipe can be made: Can I make <recipe>?\n"),
  write("  To query for all recipes that can be made: What can I make?\n"),
  flush_output(current_output).

h :- info.

%%% Interactive Loop

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
  remove_punc(L, L2),
  maplist([I,O]>>(downcase_atom(I,O)), L2, L3),
  checkquit(L3).
