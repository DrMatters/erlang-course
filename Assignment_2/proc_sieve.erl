-module(proc_sieve).

-export([generate/1, gen_print/1, sieve/0]).

%% @doc initialises sieve process with initial prime number
sieve() ->
  receive
    {send, N} ->
      sieve_leaf_node(N)
  end.

%% @doc runs sieve node which doesn't have any children (yet)
sieve_leaf_node(Divisor) ->
  receive
    {send, N} ->
      if
        N rem Divisor == 0 ->
          sieve_leaf_node(Divisor);
        true ->
          ChildPID = spawn(proc_sieve, sieve, []),
          ChildPID ! {send, N},
          sieve_parent_node(Divisor, ChildPID)
      end;
    {done, ReqPID} ->
      ReqPID ! [Divisor]
  end.
%% @doc runs sieve node which has a children node
sieve_parent_node(Divisor, ChildPID) ->
  receive
    {send, N} ->
      if
        N rem Divisor == 0 -> ok;
        true -> ChildPID ! {send, N}
      end,
      sieve_parent_node(Divisor, ChildPID);
    {done, ReqPID} ->
      ChildPID ! {done, self()},
      receive
        Primes ->
          ReqPID ! [Divisor | Primes]
      end
  end.

%% @doc sends numbers from Current to End to PID using tail recursion
send_seq(Current, End, PID) when Current > End ->
  PID ! {done, self()};
send_seq(Current, End, PID) ->
  PID ! {send, Current},
  send_seq(Current + 1, End, PID).

%% @doc generates prime numbers from 2 to MaxN
generate(MaxN) ->
  ChildPID = spawn(proc_sieve, sieve, []),
  send_seq(2, MaxN, ChildPID),
  receive
    Primes -> Primes
  end.

%% @doc generates and prints prime numbers from 2 to MaxN
gen_print(MaxN) ->
  Primes = generate(MaxN),
  lists:foreach(fun(E) -> io:format("~p~n", [E]) end, Primes).
