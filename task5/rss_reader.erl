-module(rss_reader).
-include("logging.hrl").
-define(RETRIEVE_INTERVAL, 60000).
-export([start/2, server/2]).

start(Url, QPid) ->
  spawn(?MODULE, server, [Url, QPid]).

server(Url, QPid) ->
  ?INFO("Starting rss reader, url: ~p, interval: ~p~n", [Url, ?RETRIEVE_INTERVAL]),
  case httpc:request(Url) of
    {ok, Result} ->
      ?INFO("Successfully retrieved rss feed", []),
      {_, _, Body} = Result,
      {Feed, _} = xmerl_scan:string(Body, [{xmlbase, ""}]),
      case rss_parse:is_rss2_feed(Feed) of
        false ->
          ?ERROR("exiting rss reader: invalid rss2 fedd~n", []),
          exit("Invalid rss2 feed");
        true -> rss_queue:add_feed(QPid, Feed)
      end,
      receive after ?RETRIEVE_INTERVAL -> server(Url, QPid) end;
    {error, Reason} ->
      ?ERROR("exiting rss reader: ~p~n", [Reason]),
      exit(Reason);
    _Else ->
      ?ERROR("unable to process message", []),
      exit("unable  to process message")
  end.