-module(fib).
-export([fib_p/1, fib_g/1, tail_fb/1]).

fib_p(N) ->
    if N == 0 -> 0;
       N == 1 -> 1;
       true -> fib_p(N - 1) + fib_p(N - 2)
    end.

fib_g(N) when N == 0 -> 0;
fib_g(N) when N == 1 -> 1;
fib_g(N) -> fib_g(N - 1) + fib_g(N - 2).


tail_fb(N) when N < 1 -> 0;
tail_fb(N) -> tail_fb_helper(N, 1, 0, 1).

%% @doc tail recurion for tail_fb
tail_fb_helper(N, CurrentN, _, Cur) when N == CurrentN -> Cur;
tail_fb_helper(N, CurrentN, Prev, Cur) ->
    tail_fb_helper(N, CurrentN + 1, Cur, Prev + Cur). 
