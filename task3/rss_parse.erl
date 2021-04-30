-module(rss_parse).
-include_lib("xmerl/include/xmerl.hrl").
-export([is_rss2_feed/1, get_feed_items/1, get_item_time/1, compare_feed_items/2, test/0]).

%% @doc checks if Root is a valid rss2 feed
is_rss2_feed(Root) ->
  % имя корневого атрибута = рсс
  Root#xmlElement.name == rss
    andalso
  % атрибут version у этого корневого элемента = 2.0
  % (что надо найти, в каком поле, где)
  % получить у рута элемент .attributes
    case lists:keyfind(version, #xmlAttribute.name, Root#xmlElement.attributes) of
      #xmlAttribute{value = "2.0"} -> true;
      _Else -> false
    end.

%% @doc collects all items from Root rss feed
get_feed_items(Root) -> xmerl_xpath:string("//item", Root).

%% @doc extracts pubDate as a number of seconds since 0th year
get_item_time(Item) ->
  case lists:keyfind(pubDate, #xmlElement.name, Item#xmlElement.content) of
    #xmlElement{content = [#xmlText{value = PubDate}]} ->
      % <pubDate>Tue, 27 Jan 2009 01:10:10 +0000</pubDate>
      case httpd_util:convert_request_date(PubDate) of
        bad_date -> bad_date;
        Datetime -> calendar:datetime_to_gregorian_seconds(Datetime)
      end;
    false -> bad_date
  end.

%% @doc compares recency of OldItem and NewItem
%% returns same, if items are the same
%% returns updated, if NewItems is an updated version of the OldItem
%% returns different if OldItem and NewItem are different items
compare_feed_items(OldItem, NewItem) ->
  LExtractedItem = extract_xml(OldItem),
  RExtractedItem = extract_xml(NewItem),
  comp_comparator_list(
    LExtractedItem,
    RExtractedItem,
    [
      fun compare_equal/2,
      fun compare_by_guid/2,
      fun compare_by_title/2,
      fun compare_by_link/2
    ]
  ).

%% @doc checks recency of OldItem and NewItem using the list of comparators
%% propagates updated and same result
%% fallbacks on next comparator if current comparator returned different atom
comp_comparator_list(_, _, []) -> different;
comp_comparator_list(OldItem, NewItem, [H | Rest]) ->
  case H(OldItem, NewItem) of
    updated -> updated;
    same -> same;
    different -> comp_comparator_list(OldItem, NewItem, Rest)
  end.

%% @doc comparator to compare recency of OldItem and NewItem by equality
compare_equal(OldItem, NewItem) when OldItem == NewItem -> same;
compare_equal(_, _) -> different.

%% @doc compares recency of Left and Right items using Key element of content
base_compare(Left, Right, Key) ->
  LeftKey = lists:keyfind(Key, #xmlElement.name, Left#xmlElement.content),
  RightKey = lists:keyfind(Key, #xmlElement.name, Right#xmlElement.content),
  % <title>[Renewable Energy Accounts For Largest Increase On U.S. Grid.]</title>
  if
    (LeftKey /= false) and (RightKey /= false) ->
      if
        LeftKey == RightKey -> updated;
        true -> different
      end;
    true -> different
  end.

%% @doc compares recency of OldItem and NewItem using guid content element
compare_by_guid(OldItem, NewItem) -> base_compare(OldItem, NewItem, guid).

%% @doc compares recency of OldItem and NewItem using title content element
compare_by_title(OldItem, NewItem) -> base_compare(OldItem, NewItem, title).

%% @doc compares recency of OldItem and NewItem using link content element
compare_by_link(OldItem, NewItem) -> base_compare(OldItem, NewItem, link).

%% @doc extracts only necessary information from RSS elements
extract_xml(Elem = #xmlElement{}) ->
  Elem#xmlElement{parents = [], pos = 0, xmlbase=undeclared,
    content = lists:map(fun extract_xml/1, Elem#xmlElement.content),
    attributes = lists:map(fun extract_xml/1, Elem#xmlElement.attributes)};
extract_xml(Attr = #xmlAttribute{}) ->
  Attr#xmlAttribute{parents = [], pos = 0};
extract_xml(Text = #xmlText{}) ->
  Text#xmlText{parents = [], pos = 0};
extract_xml(Comment = #xmlComment{}) ->
  Comment#xmlComment{parents = [], pos = 0};
extract_xml(Other) ->
  Other.


%% xml examples are at vova's repo: https://github.com/Mvwivs/erlang-course/tree/master/task_3
test() ->
  % xmerl_scan вываливает результат в виде структуры рекордов
  % "xmlElement",... туплей и списков
	{Rss, _} = xmerl_scan:file("test.xml"),
	Items = rss_parse:get_feed_items(Rss),
  % get_feed_items выводит объекты с разными ненужными атрибутами
  % extract_xml оставляет только нужное. Если так не делать, то
  % сравнения не будут работать (даже для одного и того же элемента)
	Old = lists:nth(3, Items),
	New = lists:nth(2, Items),
	io:format("~p~n~n~n~p~n", [Old, New]),
  rss_parse:compare_feed_items(Old, New).