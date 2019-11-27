:- dynamic have/1.
:- dynamic requires/2.
have(mushrooms).
have(salt).
have(pepper).
have(cheese).
have(bar).
have(eggs).
requires(omelette, [eggs,salt,pepper,cheese]).
requires(foo, [bar]).
