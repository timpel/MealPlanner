% Make procedures dynamic
:- dynamic have/1.
:- dynamic requires/2.

%%% Ingredients
have(mushrooms).
have(cheese).
have(eggs).
have(salt).
have(pepper).
have(sugar).
have(chocolate).

%%% Recipes
requires(omelette, [eggs,salt,pepper,cheese]).
requires(burger, [beef,buns]).
requires(cereal, [cereal,milk]).
requires(bread, [flour,salt,sugar,canola_oil,yeast]).
