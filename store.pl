% Make procedures dynamic
:- dynamic have/1.
:- dynamic requires/2.

%%% Ingredients
have(mushrooms).
have(salt).
have(pepper).
have(cheese).

%%% Recipes
requires(omelette, [eggs,salt,pepper,cheese]).
