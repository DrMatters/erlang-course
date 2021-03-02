-module(rss_queue).
-include("logging.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-define(QUEUE_TIMEOUT_MILLISECONDS, 1000).
-record(queueItem, {pubTime, item}).
-record(queueState, {items, subscribers}).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2]).
-export([start/1, start/2, add_feed/2, add_item/2, subscribe/2, unsubscribe/1, get_all/1]).

%% @doc spawns standalone rss_queue process
start(Name) -> gen_server:start({local, Name}, ?MODULE, [], []).

%% @doc spawns rss_queue process linked with rss_reader
start(Name, URL) -> gen_server:start({local, Name}, ?MODULE, [URL], []).

%% @doc performs initialisation of rss_queue process
init([]) -> ?INFO("Starting standalone rss queue~n", []), {ok, #queueState{items = [], subscribers = maps:new()}};

%% @doc performs initialisation of rss_queue linked with rss_reader
init(Url) ->
  ?INFO("Starting rss queue and rss reader, url: ~p~n", [Url]),
  ReaderPID = rss_reader:start(Url, self()),
  link(ReaderPID),
  {ok, #queueState{items = [], subscribers = maps:new()}}.

%% @doc sends item to queue
add_item(ID, Item) when is_pid(ID) or is_atom(ID) ->
  gen_server:cast(ID, {add_item, Item}).

%% @doc sends all items from RSS2Feed to queue
add_feed(ID, RSS2Feed) when is_pid(ID) or is_atom(ID) ->
  FeedItems = rss_parse:get_feed_items(RSS2Feed),
  lists:foreach(fun(Item) -> add_item(ID, Item) end, FeedItems),
  ok.

%% @doc retrieves all items from queue
get_all(QPid) when is_pid(QPid) -> gen_server:call(QPid, {get_all}, ?QUEUE_TIMEOUT_MILLISECONDS);
get_all(QPid) when is_pid(QPid) -> gen_server:call(QPid, {get_all}, ?QUEUE_TIMEOUT_MILLISECONDS).

subscribe(Queue1, Queue2) when is_pid(Queue1) ->
  gen_server:call(Queue2, {subscribe, Queue1}, ?QUEUE_TIMEOUT_MILLISECONDS);

subscribe(Queue1, Queue2) when is_atom(Queue1) ->
  case whereis(Queue1) of
    undefined -> exit({noproc, {?MODULE, subscribe, [Queue1]}});
    QPid -> subscribe(QPid, Queue2)
  end.

unsubscribe(Queue) when is_pid(Queue) or is_atom(Queue) -> gen_server:cast(Queue, {unsubscribe, self()}).

handle_call({get_all}, _, QueueState) -> {reply, get_items_from_state(QueueState), QueueState};
handle_call({subscribe, QPid}, _, QueueState) ->
  case handle_subscribe(QueueState, QPid) of
    {error, already_subscribed} -> {reply, {error, already_subscribed}, QueueState};
    NewState -> {reply, get_items_from_state(NewState), NewState}
  end.

handle_cast({unsubscribe, QPid}, QueueState) -> {noreply, handle_unsubscribe(QueueState, QPid)};
handle_cast({add_item, RSSItem = #xmlElement{name = item}}, QueueState) ->
  {noreply, handle_add_item(QueueState, RSSItem)}.

%% @doc handles add_item message, adds RSSItem to Queue
handle_add_item(#queueState{items = Items, subscribers = Subscribers} = QueueState, RSSItem) ->
%%  Xml = lists:flatten(xmerl:export_simple([RSSItem], xmerl_xml)),
%%  io:format("~s~n~n", [Xml]),
  case get_item_recency_state(Items, RSSItem) of
    {same, _} ->
      log_item_guid(same, RSSItem),
      QueueState;
    {updated, FeedItem} ->
      log_item_guid(updated, RSSItem),
      QueueWithoutOldItem = lists:delete(FeedItem, Items),
      UpdatedQueueState = #queueState{items = QueueWithoutOldItem, subscribers = Subscribers},
      handle_new_item(UpdatedQueueState, RSSItem);
    different ->
      log_item_guid(different, RSSItem),
      handle_new_item(QueueState, RSSItem)
  end.

%% @doc performs insertion of NewItem to Queue
handle_new_item(#queueState{items = Queue, subscribers = Subscribers}, NewItem) ->
  NewQueue = add_item_to_server_queue(Queue, NewItem),
  send_item_to_queues(NewItem, Subscribers),
  #queueState{items = NewQueue, subscribers = Subscribers}.

%% @doc subscribes QPid on Queue
handle_subscribe(#queueState{items = Queue, subscribers = Subscribers}, QPid) when is_pid(QPid) ->
  Ref = erlang:monitor(process, QPid),
  UpdatedSubscribers = maps:put(QPid, Ref, Subscribers),
  lists:foreach(fun(#queueItem{item = Item}) -> add_item(QPid, Item) end, Queue),
  #queueState{items = Queue, subscribers = UpdatedSubscribers}.

%% @doc unsubscribes QPid from Queue
handle_unsubscribe(#queueState{subscribers = Subscribers, items = Items} = QueueState, QPid) when is_pid(QPid) ->
  case maps:take(QPid, Subscribers) of
    error -> QueueState;
    {Ref, UpdatedSubscribers} ->
      erlang:demonitor(Ref),
      #queueState{items = Items, subscribers = UpdatedSubscribers}
  end.

log_item_guid(State, Item) ->
  case rss_parse:get_item_guid(Item) of
    bad_guid -> {error, bad_guid};
    Guid -> ?INFO("~s item, guid = ~s", [State, Guid]), ok
  end.


%% @doc performs insertion of Item to Queue
add_item_to_server_queue(Queue, Item) ->
  case rss_parse:get_item_time(Item) of
    bad_date ->
      case rss_parse:get_item_guid(Item) of
        bad_guid -> throw("Invalid RSS Item");
        Guid ->
          ?WARN("elem {guid = ~s} had bad_date", [Guid]),
          Queue
      end;
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

get_items_from_state(#queueState{items = Items}) ->
  GetItem = fun(#queueItem{item = Item}) -> Item end,
  lists:map(GetItem, Items).

%% @doc broadcasts Item to all subscribers from QueuePIDs
send_item_to_queues(Item, QueuePIDs) ->
  SubsList = maps:keys(QueuePIDs),
  lists:foreach(fun(SubPID) -> add_item(SubPID, Item) end, SubsList).