-module(proc_sieve).

-export([generate/1, gen_print/1, sieve/0]).

%% @doc initialises sieve process with initial prime number
sieve() ->
  receive  %% receive и паттерн матчинг по сообщению
    {send, N} ->
      sieve_last_node(N)
  end.

%% @doc runs sieve node which doesn't have any children (yet)
sieve_last_node(Divisor) ->
  receive
    {send, N} ->
      if
        N rem Divisor == 0 ->
          sieve_last_node(Divisor);
        true ->
          ChildPID = spawn(proc_sieve, sieve, []),
          ChildPID ! {send, N},
          sieve_intermediate_node(Divisor, ChildPID)
      end;
    {done, ReqPID} ->
      ReqPID ! [Divisor]
  end.
%% @doc runs sieve node which has a children node
sieve_intermediate_node(Divisor, ChildPID) ->
  receive
    {send, N} ->
      if
        N rem Divisor == 0 -> ok;
        true -> ChildPID ! {send, N}
      end,
      sieve_intermediate_node(Divisor, ChildPID);
    {done, ReqPID} ->
      ChildPID ! {done, self()},
      receive
        Primes ->
          ReqPID ! [Divisor | Primes]
      end
  end.

%% @doc sends numbers from Current to End to PID using tail recursion
send_seq(Current, End, BaseSievePID) when Current > End ->
  BaseSievePID ! {done, self()}; %% self() твой собственный pid, в него посылаем сообщение с рез-ми работы
send_seq(Current, End, BaseSievePID) ->
  BaseSievePID ! {send, Current},
  send_seq(Current + 1, End, BaseSievePID).

%% @doc generates prime numbers from 2 to MaxN
generate(MaxN) ->
  BaseSievePID = spawn(proc_sieve, sieve, []),
  send_seq(2, MaxN, BaseSievePID),
  receive
    Primes -> Primes
  end.

%% @doc generates and prints prime numbers from 2 to MaxN
gen_print(MaxN) ->
  Primes = generate(MaxN),
  lists:foreach(fun(E) -> io:format("~p~n", [E]) end, Primes).
