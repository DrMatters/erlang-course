-module(rss_queue).
-define(QUEUE_TIMEOUT_MILLISECONDS, 1000).
-record(queue_item, {pubTime, item}).
-export([server/0, start/0]).

start() -> spawn(?MODULE, server, []).

add_item(QPid, Item) when is_pid(QPid) -> QPid ! {add_item, Item}, ok.

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

server() -> server_queue([]).

server_queue(Queue) ->
  receive
    {add_item, RSSItem} ->
      NewQueue = handle_add_item(Queue, RSSItem),
      server_queue(NewQueue);
    {get_all, ReqPid} ->
      Items = lists:map(fun(#queue_item{item = Item}) -> Item end, Queue),
      ReqPid ! Items,
      server_queue(Queue)
  end.

handle_add_item(Queue, RSSItem) ->
  case search_item(Queue, RSSItem) of
    {same, _} -> Queue;
    {updated, FeedItem} ->
      QueueWithoutOldItem = lists:delete(FeedItem, Queue),
      add_item_to_queue(QueueWithoutOldItem, RSSItem);
    different -> add_item_to_queue(Queue, RSSItem)
  end.

add_item_to_queue(Queue, Item) ->
  PubTime = rss_parse:get_item_time(Item),
  QueueItem = #queue_item{pubTime = PubTime, item = Item},
  add_item_to_sorted_queue(Queue, QueueItem).

add_item_to_sorted_queue([], Item) -> [Item];
add_item_to_sorted_queue([H | T], Item) ->
  CompareByPubTime = fun(#queue_item{pubTime = A}, #queue_item{pubTime = B}) -> A =< B end,
  lists:sort(CompareByPubTime, [Item, H | T])
.

search_item([], _) -> different;
search_item([FeedItem | T], Item) ->
  case rss_parse:compare_feed_items(FeedItem#queue_item.item, Item) of
    same -> {same, FeedItem};
    updated -> {updated, FeedItem};
    different -> search_item(T, Item)
  end.