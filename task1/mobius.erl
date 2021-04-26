-module(mobius).
-export([is_prime/1, prime_factors/1, twos_factors/1, is_square_multiple/1, find_square_multiples/2]).
-import(math, [sqrt/1]).

is_prime(N) -> is_prime_tail(sqrt(N), N, 2).

is_prime_tail(Threshold, _, Divisor) when Divisor > Threshold -> true;
is_prime_tail(Threshold, N, 2) -> N rem 2 > 0 andalso is_prime_tail(Threshold, N, 3);
is_prime_tail(Threshold, N, Divisor) ->
  if N rem Divisor == 0 -> false;
    true -> is_prime_tail(Threshold, N, Divisor + 2)
  end.

prime_factors(N) ->
  {M, Twos} = twos_factors(N),
  prime_factors_tail(M, 3, Twos).

prime_factors_tail(N, Divisor, Factors) ->
  if N =< 1 -> Factors;
    N rem Divisor == 0 -> prime_factors_tail(N div Divisor, Divisor, [Divisor | Factors]);
    true -> prime_factors_tail(N, Divisor + 2, Factors)
  end.

twos_factors(N) ->
  twos_factors_tail(N, []).

twos_factors_tail(N, Factors) ->
  if N =< 1 -> {N, Factors};
    N rem 2 == 0 -> twos_factors_tail(N div 2, [2 | Factors]);
    true -> {N, Factors}
  end.

is_square_multiple(N) ->
  Factors = prime_factors(N),
  UniqueFactors = sets:from_list(Factors),
  length(Factors) > sets:size(UniqueFactors).

find_sublist_of_length(_, _, []) -> [];
find_sublist_of_length(Pred, Len, [H | T]) ->
  Sub = lists:sublist([H | T], Len),
  LenSub = length(Sub),
  Passed = lists:all(Pred, Sub),
  if LenSub < Len -> fail;
    Passed -> Sub;
    true -> find_sublist_of_length(Pred, Len, T)
  end.

find_square_multiples(Count, MaxN) ->
  Checked = [
    {X, is_square_multiple(X)} ||
    X <- lists:seq(2, MaxN + 2)
  ],
  Triplet = find_sublist_of_length(fun({_, Check}) -> Check end, Count, Checked),
  case Triplet of
    fail -> fail;
    [{E, _} | _] -> E
  end.