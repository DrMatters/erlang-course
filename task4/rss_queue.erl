-module(rss_queue).
-define(QUEUE_TIMEOUT_MILLISECONDS, 1000).
-record(queue_item, {pubTime, item}).
-export([server/0, start/0, add_feed/2, get_all/1, test/0]).

%% @doc spawns server process
% ?MODULE - макрос, текущий модуль
start() -> spawn(?MODULE, server, []).

%% @doc sends item to queue
add_item(QPid, Item) when is_pid(QPid) -> QPid ! {add_item, Item}, ok.

%% @doc sends all items from RSS2Feed to queue
add_feed(QPid, RSS2Feed) when is_pid(QPid) ->
  FeedItems = rss_parse:get_feed_items(RSS2Feed),
  lists:foreach(fun(Item) -> add_item(QPid, Item) end, FeedItems),
  ok.

%% @doc retrieves all items from queue
get_all(QPid) when is_pid(QPid) ->
  QPid ! {get_all, self()},
  receive
    Items -> {ok, Items}
  after ?QUEUE_TIMEOUT_MILLISECONDS ->
    {error, timeout}
  end.

%% @doc launches empty server_queue
server() -> server_queue([]).

%% @doc contains main server loop
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

%% @doc handles add_item message, adds RSSItem to Queue
handle_add_item(Queue, RSSItem) ->
  case get_item_recency_state(Queue, RSSItem) of
    {same, _} -> Queue;
    {updated, FeedItem} ->
      QueueWithoutOldItem = lists:delete(FeedItem, Queue),
      add_item_to_server_queue(QueueWithoutOldItem, RSSItem);
    different -> add_item_to_server_queue(Queue, RSSItem)
  end.

%% @doc performs insertion of Item to Queue
add_item_to_server_queue(Queue, Item) ->
  PubTime = rss_parse:get_item_time(Item),
  QueueItem = #queue_item{pubTime = PubTime, item = Item},
  add_item_to_sorted_list(Queue, QueueItem).

%% @doc performs insertion of Item to sorted list, maintains list order by pubTime
add_item_to_sorted_list([], Item) -> [Item];
add_item_to_sorted_list([H | T], Item) ->
  % comparator
  CompareByPubTime = fun(#queue_item{pubTime = A}, #queue_item{pubTime = B}) -> A =< B end,
  lists:sort(CompareByPubTime, [Item, H | T]) .

%% @doc retrieves recency state of Item from lists of items
get_item_recency_state([], _) -> different;
get_item_recency_state([FeedItem | T], Item) ->
  case rss_parse:extract_and_compare(FeedItem#queue_item.item, Item) of
    same -> {same, FeedItem};
    updated -> {updated, FeedItem};
    different -> get_item_recency_state(T, Item)
  end.

test() -> 
  QPID = start(),
  {Rss1, _} = xmerl_scan:file("digg-science-rss1.xml"),
  {Rss2, _} = xmerl_scan:file("digg-science-rss2.xml"),
  rss_queue:add_feed(QPID, Rss1),
  rss_queue:add_feed(QPID, Rss2),
  {_, Items} = rss_queue:get_all(QPID),
  Items.
