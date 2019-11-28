/*
------- A Prolog ingredient Tracker and Recipe Book ---------
----------- By Tim Pellissier and Jordan Kroll --------------
*/

% Make procedures dynamic
:- dynamic have/1.
:- dynamic requires/2.
:- dynamic current_user/1.

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

% The current user. Starts as 'default'.
current_user('default').

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
  \+ have(_),
  write("You currently don't have any ingredients!\n"),
  write("You can add an ingredient by writing: add <ingredient>\n"),
  flush_output(current_output), !.

input(['ingredients']) :-
  findall(X, have(X), List_of_ingredients),
  write("The current ingredients are: "),
  write(List_of_ingredients), write(".\n"), flush_output(current_output), !.

input(['what', 'do', 'i', 'have']) :-
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
  \+ requires(_, _),
  write("You currently don't have any recipes!\n"),
  write("You can add a recipe by writing: <recipe>: <ingredient>\n"),
  flush_output(current_output), !.

input(['list', 'recipes']) :-
  findall((X,Y), requires(X,Y), Z),
  write("Your recipes are: "),
  write(Z), write(".\n"), flush_output(current_output), !.

input(['list', X, 'recipes']) :-
  det_for_plural(X),
  input(['list', 'recipes']), !.

input(['list', X, Y, 'recipes']) :-
  det_for_plural(X),
  det(Y),
  input(['list', 'recipes']), !.

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

% Querying for an individual recipe.
input(['recipe', X]) :-
  requires(X, L),
  write("The recipe for "),
  write(X), write(" has the following ingredients: "),
  write(L), write("\n"),
  flush_output(current_output), !.

input(['recipe', X]) :-
  \+ requires(X, _),
  write("You do not have a recipe for "),
  write(X), write(".\nTo add a recipe write: "),
  write(X), write(": <ingredient> ...\n"),
  flush_output(current_output), !.

input(['get','recipe', X]) :- input(['recipe', X]).
input(['get', D,'recipe', X]) :-
  det(D),
  input(['recipe', X]).
input(['get', D,'recipe', 'for', X]) :- input(['get', D,'recipe', X]).
input(['get', D,'recipe', 'for', D2, X]) :-
  det(D2),
  input(['get', D,'recipe', X]).
input(['what','is', D, 'recipe', 'for', X]) :- input(['get', D,'recipe', X]).

% Querying if a recipe can be made.
input(['can', 'i', 'make', X]) :-
  \+ requires(X, _),
  write("You do not have a recipe for "),
  write(X), write(".\nTo add a recipe write: "),
  write(X), write(": <ingredient> ...\n"),
  flush_output(current_output), !.

input(['can', 'i', 'make', X]) :-
  requires(X, Ings),
  stillneed(X, Ings, []),
  write("You have all the ingredients for "),
  write(X), write(".\n"), flush_output(current_output), !.

input(['can', 'i', 'make', X]) :-
  requires(X, Ings),
  stillneed(X, Ings, L),
  write("No, you are missing the following ingredients: "),
  write(L), write(".\n"), flush_output(current_output), !.

input(['can', 'i', 'make', Y, X]) :-
  det(Y),
  input(['can', 'i', 'make', X]).

% Querying for potential meals.
input(['what', 'can', 'i', 'make']) :-
  findall(X, (requires(X,Y), canmake(X,Y)), Z),
  write("The items you can make are: "),
  write(Z), write(".\n"), flush_output(current_output), !.

% Saving to file.
input(['save']) :-
  save, !.

input(['save','store']) :- save, !.

% Loading a file.
input(['load']) :- load, !.
input(['load','store']) :- load('store'), !.

% Querying user
input(['user']) :-
  current_user(U),
  write("The current user is: "), write(U),
  write(".\n"), flush_output(current_output), !.

input(['current','user']) :- input(['user']).
input(['the','current','user']) :- input(['user']).
input(['get','current','user']) :- input(['user']).
input(['get','the','user']) :- input(['user']).
input(['get','the','current','user']) :- input(['user']).
input(['who','is','the','current','user']) :- input(['user']).

% Changing user (login/logout)
input(['login', U]) :-
  current_user(U),
  write("User "), write(U),
  write(" is already logged in.\n"), flush_output(current_output), !.

input(['login', U]) :-
  current_user(X),
  retractall(current_user(_)),
  assert(current_user(U)),
  save_if_not_empty(X),
  retractall(have(_)),
  retractall(requires(_,_)),
  write("User "), write(U),
  write(" has been logged in.\n"), flush_output(current_output),
  load, !.

% Fallback case if current_user is manually retracted.
input(['login', U]) :-
  retractall(current_user(_)),
  assert(current_user(U)),
  retractall(have(_)),
  retractall(requires(_,_)),
  write("User "), write(U),
  write(" has been logged in.\n"), flush_output(current_output),
  load, !.

input(['login']) :- input(['login', 'default']).
input(['logout']) :- input(['login', 'default']).

input(['logout', U]) :-
  current_user(U),
  input(['login', 'default']), !.
input(['logout', U]) :-
  write("Logout failed. User "), write(U),
  write(" is not currently logged in.\n"), flush_output(current_output), !.

% Asking for info
input(['h']) :- info, !.
input(['help']) :- info, !.
input(['info']) :- info, !.
input(['what','can','i','ask']) :- info, !.
input(['what','can','i','do']) :- info, !.
input(['list','queries']) :- list_queries, !.
input(['list','commands']) :- list_commands, !.

% Politeness.
input(['please' | T]) :- input(T), !.

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
  current_user(U),
  save(U), !.

save('default') :- save('store'), !.

save(N) :-
  atom_concat(N,'.pl',Filename),
  open(Filename,write,Stream),
  write(Stream, "% Make procedures dynamic\n"),
  write(Stream, ":- dynamic have/1.\n"),
  write(Stream, ":- dynamic requires/2.\n"),
  findall(X, have(X), Ingredients),
  write(Stream, "\n%%% Ingredients\n"),
  saveItems(Stream, Ingredients),
  findall(Y, requires(Y, _), Recipes),
  write(Stream, "\n%%% Recipes\n"),
  saveRecipes(Stream, Recipes),
  close(Stream),
  write("Data for current user has been saved to: "),
  write(Filename), write(".\n"), flush_output(current_output), !.

saveItems(_, []).
saveItems(Stream, [H | T]) :-
  write(Stream, "have("),
  write(Stream, H),
  write(Stream, ").\n"),
  saveItems(Stream, T), !.

saveRecipes(_, []).
saveRecipes(Stream, [H | T]) :-
  write(Stream, "requires("),
  write(Stream, H),
  write(Stream, ", "),
  requires(H, L),
  write(Stream, L),
  write(Stream, ").\n"),
  saveRecipes(Stream, T), !.

save_if_not_empty(U) :-
  have(_),
  requires(_, _),
  save(U), !.

save_if_not_empty(U) :-
  write("Nothing to save for user "),
  write(U), write(".\n"),
  flush_output(current_output), !.

%%% Loading from file.
load :-
  current_user(U),
  load(U), !.

load('default') :- load('store'), !.

load(U) :-
  atom_concat(U,'.pl',Filename),
  exists_file(Filename),
  [U],
  write("Saved data has been successfully loaded from file: "),
  write(Filename), write(".\n"),
  flush_output(current_output), !.

load(U) :-
  atom_concat(U,'.pl',Filename),
  \+ exists_file(Filename),
  write("No file exists for user: "), write(U), write(".\n"),
  write("If you have logged in a new user, ignore this warning.\n"),
  flush_output(current_output), !.


%%% Info

info :-
  write("\n----Welcome to the help page for the Meal Planner!----\n"),
  write("To begin issuing commands or queries type: start. or i.\n"),
  list_commands,
  list_queries,
  write("\nNote that there are variants of the above commands and queries.\n"),
  write("All commands and queries are case insensitive.\n"),
  write("All punctuation other than ':' is ignored.\n"),
  flush_output(current_output).

list_commands :-
  write("\nCommands:\n"),
  write("  To add ingredients: add <ingredient> ...\n"),
  write("  To remove ingredients: remove <ingredient> ...\n"),
  write("  To add a recipe: <recipe>: <ingredient> ...\n"),
  write("      To modify a recipe: <recipe>: <ingredient> ...\n"),
  write("  To log in as a particular user: login <username>\n"),
  write("      Logging in as a user automatically saves (to file) the state of the\n"),
  write("      previous user and loads the state of the newly logged in user.\n"),
  write("  To logout (log back in as default user): logout\n"),
  flush_output(current_output).

list_queries :-
  write("\nQueries:\n"),
  write("  To get a list of all ingredients: What do I have?\n"),
  write("      Alternate query: list ingredients\n"),
  write("  To query for a particular ingredient: Do I have <ingredient>?\n"),
  write("  To get a list of all recipes: What are my recipes?\n"),
  write("      Alternate query: list recipes\n"),
  write("  To query for a particular recipe: What is the recipe for <recipe>?\n"),
  write("  To query for whether a recipe can be made: Can I make <recipe>?\n"),
  write("  To query for all recipes that can be made: What can I make?\n"),
  flush_output(current_output).

h :- info.

%%% Interactive Loop

% checkquit loop
checkquit(['ex' | _]) :-
  current_user(U),
  save_if_not_empty(U), !.
checkquit(['exit' | _]) :- checkquit(['ex' | _]), !.
checkquit(L) :-
  input(L),
  q.

start :- i.
i :-
  retractall(current_user(_)),
  assert(current_user('default')),
  retractall(have(_)),
  retractall(requires(_,_)),
  load('store'),
  q.

q :-
  write("\nAsk a query, update recipes, or update ingredients: "), flush_output(current_output),
  readln(L),     % L is an array of words
  remove_punc(L, L2),
  maplist([I,O]>>(downcase_atom(I,O)), L2, L3),
  checkquit(L3).
