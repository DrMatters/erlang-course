-module(rss_queue).
-include("logging.hrl").
-define(QUEUE_TIMEOUT_MILLISECONDS, 1000).
-record(queueItem, {pubTime, item}).
-record(queueState, {items, subscribers}).
-export([start/0, start/1, init/0, init/1, add_feed/2, get_all/1]).

start() -> spawn(?MODULE, init, []).

start(URL) -> spawn(?MODULE, init, [URL]).

init() ->
  ?INFO("Starting standalone rss queue~n", []),
  server().

init(Url) ->
  ?INFO("Starting rss queue and rss reader, url: ~p~n", [Url]),
  ReaderPID = rss_reader:start(Url, self()),
  link(ReaderPID),
  server().

add_item(QPid, Item) when is_pid(QPid) ->
  QPid ! {add_item, Item},
  ok.

add_feed(QPid, RSS2Feed) when is_pid(QPid) ->
  FeedItems = rss_parse:get_feed_items(RSS2Feed),
  lists:foreach(fun(Item) -> add_item(QPid, Item) end, FeedItems),
  ok.

get_all(QPid) when is_pid(QPid) ->
  QPid ! {get_all, self()},
  receive
    Items -> {ok, Items}
  after ?QUEUE_TIMEOUT_MILLISECONDS ->
    {error, timeout}
  end.

server() -> server_loop(#queueState{items = [], subscribers = sets:new()}).

server_loop(QueueState) ->
  NewState =
    receive
      {add_item, RSSItem} ->
        ?INFO("received add_item message~n", []),
        handle_add_item(QueueState, RSSItem);
      {get_all, ReqPid} ->
        ?INFO("received get_all message~n", []),
        GetItem = fun(#queueItem{item = Item}) -> Item end,
        Items = lists:map(GetItem, QueueState#queueState.items),
        ReqPid ! Items,
        QueueState;
      {subscribe, QPid} ->
        ?INFO("received {subscribe, ~p} message~n", [QPid]),
        handle_subscribe(QueueState, QPid);
      {unsubscribe, QPid} ->
        ?INFO("received {unsubscribe, ~p} message~n", [QPid]),
        handle_unsubscribe(QueueState, QPid);
      {'DOWN', _, _, QPid, _} ->
        ?WARN("received {DOWN, ~p} message~n", [QPid]),
        remove_subscriber(QueueState, QPid)
    end,
  server_loop(NewState).

handle_add_item(#queueState{items = Items, subscribers = Subscribers} = QueueState, RSSItem) ->
  case get_item_recency_state(Items, RSSItem) of
    {same, _} ->
      ?INFO("got same item~n", []),
      QueueState;
    {updated, FeedItem} ->
      ?INFO("got updated item~n", []),
      QueueWithoutOldItem = lists:delete(FeedItem, Items),
      UpdatedQueueState = #queueState{items = QueueWithoutOldItem, subscribers = Subscribers},
      handle_new_item(UpdatedQueueState, RSSItem);
    different ->
      ?INFO("got new item~n", []),
      handle_new_item(QueueState, RSSItem)
  end.

handle_new_item(#queueState{items = Queue, subscribers = Subscribers}, NewItem) ->
  NewQueue = add_item_to_server_queue(Queue, NewItem),
  send_item_to_queues(NewItem, Subscribers),
  #queueState{items = NewQueue, subscribers = Subscribers}.

handle_subscribe(#queueState{items = Queue, subscribers = Subscribers}, QPid) when is_pid(QPid) ->
  UpdatedSubscribers = sets:add_element(QPid, Subscribers),
  erlang:monitor(process, QPid),
  lists:foreach(fun(#queueItem{item = Item}) -> add_item(QPid, Item) end, Queue),
  #queueState{items = Queue, subscribers = UpdatedSubscribers}.

handle_unsubscribe(#queueState{subscribers = Subscribers} = QueueState, QPid) when is_pid(QPid) ->
  case sets:is_element(QPid, Subscribers) of
    true -> remove_subscriber(QueueState, QPid);
    false -> QueueState
  end.

remove_subscriber(#queueState{items = Queue, subscribers = Subscribers}, QPid) ->
  UpdatedSubscribers = sets:del_element(QPid, Subscribers),
  erlang:demonitor(QPid),
  #queueState{items = Queue, subscribers = UpdatedSubscribers}.

add_item_to_server_queue(Queue, Item) ->
  case rss_parse:get_item_time(Item) of
    bad_date ->
      ?ERROR("~p had bad_date~n", [Item]),
      throw("Item has bad_date");
    PubTime ->
      QueueItem = #queueItem{pubTime = PubTime, item = Item},
      add_item_to_sorted_list(Queue, QueueItem)
  end.

add_item_to_sorted_list([], Item) -> [Item];
add_item_to_sorted_list([H | T], Item) ->
  CompareByPubTime = fun(#queueItem{pubTime = A}, #queueItem{pubTime = B}) -> A =< B end,
  lists:sort(CompareByPubTime, [Item, H | T]).

get_item_recency_state([], _) -> different;
get_item_recency_state([FeedItem | T], Item) ->
  case rss_parse:compare_feed_items(FeedItem#queueItem.item, Item) of
    same -> {same, FeedItem};
    updated -> {updated, FeedItem};
    different -> get_item_recency_state(T, Item)
  end.

send_item_to_queues(Item, QueuePIDs) ->
  SubsList = sets:to_list(QueuePIDs),
  lists:foreach(fun(SubPID) -> add_item(SubPID, Item) end, SubsList).