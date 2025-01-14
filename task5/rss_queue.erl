-module(rss_queue).
-include("logging.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-define(QUEUE_TIMEOUT_MILLISECONDS, 1000).
%% queueItem - pubtime для удобной сортировки
-record(queueItem, {pubTime, item}).
-record(queueState, {items, subscribers}).
-export([start/0, start/1, init/0, init/1, add_feed/2, get_all/1, test/0]).

%% @doc spawns standalone rss_queue process
start() -> spawn(?MODULE, init, []).

%% @doc spawns rss_queue process linked with rss_reader
start(URL) -> spawn(?MODULE, init, [URL]).

%% @doc performs initialisation of rss_queue process
init() ->
  ?INFO("Starting standalone rss queue~n", []),
  server().

%% @doc performs initialisation of rss_queue linked with rss_reader
init(Url) ->
  ?INFO("Starting rss queue and rss reader, url: ~p~n", [Url]),
  ReaderPID = rss_reader:start(Url, self()),
  link(ReaderPID),
  server().

%% @doc sends item to queue
add_item(QPid, Item) when is_pid(QPid) ->
  QPid ! {add_item, Item},
  ok.

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
server() -> server_loop(#queueState{items = [], subscribers = maps:new()}).

%% @doc contains main server loop
server_loop(QueueState) ->
  NewState =
    receive
      {add_item, RSSItem} ->
        ?INFO("received add_item message~n", []),
        handle_add_item(QueueState, RSSItem);
      {get_all, ReqPid} ->
        ?INFO("received get_all message~n", []),
        Items = lists:map(
          fun(#queueItem{item = Item}) -> Item end,
          QueueState#queueState.items
        ),
        ReqPid ! Items,
        QueueState;
      {subscribe, QPid} ->
        ?INFO("received {subscribe, ~p} message~n", [QPid]),
        handle_subscribe(QueueState, QPid);
      {unsubscribe, QPid} ->
        ?INFO("received {unsubscribe, ~p} message~n", [QPid]),
        remove_subscriber(QueueState, QPid);
      {'DOWN', _, _, QPid, _} ->
        ?WARN("received {DOWN, ~p} message~n", [QPid]),
        remove_subscriber(QueueState, QPid)
    end,
  server_loop(NewState).

%% @doc handles add_item message, adds RSSItem to Queue
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

%% @doc performs insertion of NewItem to Queue
handle_new_item(#queueState{items = Queue, subscribers = Subscribers}, NewItem) ->
  NewQueue = add_item_to_server_queue(Queue, NewItem),
  send_item_to_queues(NewItem, Subscribers),
  #queueState{items = NewQueue, subscribers = Subscribers}.

%% @doc subscribes QPid on Queue
handle_subscribe(#queueState{items = Queue, subscribers = Subscribers}, QPid) when is_pid(QPid) ->
  SubRef = erlang:monitor(process, QPid),
  % позволяет получать сообщения о том, что другой процесс выключается, перезагружается и т.п.
  UpdatedSubscribers = maps:put(QPid, SubRef, Subscribers),

  % Паттерн матчинг
  % t = (1, 2, 3)
  % (a, b, c) = t

  % nt = NamedTuple(a=0, b=1)
  % nt_obj = nt(a='big', b='energy')
  % nt(a=r, b=s) = nt_obj
  % # вынимаем одно поле
  % nt(a=f,) = nt_obj
  % # еще вот так можно
  % nt(a=nt(a=q,)) = nt_obj
  % # можно матчить списки
  % (где-то в коде несколько уровней вложенности и там список)

  %% создаю лямбду, которая в кач-ве аргумента принимает рекорд queueItem
  %% аргумент этой лямбды деструктурируется: поле item присваеваем в переменную Item
  % def function(queue_item):
  %   if isinstance(queue_item, NamedTuple('queue_item')):
  %     Item = queue_item.item
  %% можно матчить аргументы по вызовам конструкторов

  lists:foreach(fun(#queueItem{item = Item}) -> add_item(QPid, Item) end, Queue),
  #queueState{items = Queue, subscribers = UpdatedSubscribers}.

%% @doc remove QPid subscriber from Queue
remove_subscriber(#queueState{items = Queue, subscribers = Subscribers} = QueueState, QPid) ->
  case maps:take(QPid, Subscribers) of
    {Ref, UpdatedSubscribers} ->
      erlang:demonitor(Ref),
      #queueState{items = Queue, subscribers = UpdatedSubscribers};
    error -> QueueState
  end.

%% @doc performs insertion of Item to Queue
add_item_to_server_queue(Queue, Item) ->
  case rss_parse:get_item_time(Item) of
    bad_date ->
      throw("Item has bad_date");
    PubTime ->
      QueueItem = #queueItem{pubTime = PubTime, item = Item},
      add_item_to_sorted_list(Queue, QueueItem)
  end.

%% @doc performs insertion of Item to sorted list, maintains list order by pubTime
add_item_to_sorted_list([], Item) -> [Item];
add_item_to_sorted_list([H | T], Item) ->
  CompareByPubTime = fun(#queueItem{pubTime = A}, #queueItem{pubTime = B}) -> A =< B end,
  lists:sort(CompareByPubTime, [Item, H | T]).

%% @doc retrieves recency state of Item from lists of items
get_item_recency_state([], _) -> different;
get_item_recency_state([FeedItem | T], Item) ->
  case rss_parse:compare_feed_items(FeedItem#queueItem.item, Item) of
    same -> {same, FeedItem};
    updated -> {updated, FeedItem};
    different -> get_item_recency_state(T, Item)
  end.

%% @doc broadcasts Item to all subscribers from Subscribes
send_item_to_queues(Item, Subscribes) ->
  SubsPIDs = maps:keys(Subscribes),
  lists:foreach(fun(SubPID) -> add_item(SubPID, Item) end, SubsPIDs).

test() ->
	inets:start(),
	ssl:start(),
	CnnPid = start("http://rss.cnn.com/rss/cnn_topstories.rss"),
	BbcPid = start("http://newsrss.bbc.co.uk/rss/newsonline_world_edition/front_page/rss.xml"),
	VedomostiPid = start("https://www.vedomosti.ru/rss/rubric/economics"),
	NewsPid = start(),
	AllPid = start(),
	CnnPid ! {subscribe, NewsPid},
	BbcPid ! {subscribe, NewsPid},
	VedomostiPid ! {subscribe, AllPid},
	NewsPid ! {subscribe, AllPid},
	AllPid.
