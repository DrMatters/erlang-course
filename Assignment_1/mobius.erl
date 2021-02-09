-module(mobius).
-export([is_prime/1, prime_factors/1]).
-import(math, [sqrt/1]).
-import(lists, [filter/2, any/2]).

is_prime(N) ->
    if N rem 2 == 0 -> false; 
       true -> not any(
          fun (Num) -> (Num * Num =< N) andalso (N rem Num == 0) end, 
          lists:seq(3, N, 2))
    end.

prime_factors(N) -> 
    filter(fun is_prime/1, factors(N)).

factors(N) -> 
    [ X || 
      X <- lists:seq(2, N),
      N rem X == 0
    ].
