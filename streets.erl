-module(streets).
-export([
  fill_db/1,
  ways_to_file/4,
  analyze/1,
  generate_style/1,
  generate_filter/1,
  with_style/3,
  has_key/1,
  fill_with/1
]).
-include("streets.hrl").
-include("xml_stream.hrl").

-record(state, {
  count = 0,
  way_count = 0
}).

has_key(K) ->
  fun(Tags) -> proplists:is_defined(K, Tags) end.

fill_with(C) ->
  fun(Tags) -> [ {"fill", C} ] end.

with_style(FileName, ImageWidth, StyleDef) ->
  Style = generate_style(StyleDef),
  Predicate = generate_filter(StyleDef),
  ways_to_file(FileName, ImageWidth, Predicate, Style).

generate_style(StyleDef) ->
  fun(Tags) ->
    apply_style(Tags, StyleDef)
  end.

generate_filter(StyleDef) ->
  fun(Tags) ->
    apply_filter(Tags, StyleDef)
  end.

apply_filter(Tags, [ { Key, _Style } | Rest ]) when erlang:is_list(Key) ->
  case proplists:is_defined(Key, Tags) of
    true ->
      true;
    false ->
      apply_filter(Tags, Rest)
  end;
apply_filter(Tags, [ { {Key, Value}, _Style } | Rest ]) ->
  case proplists:get_value(Key, Tags) of
    Value ->
      true;
    _ ->
      apply_filter(Tags, Rest)
  end;
apply_filter(Tags, []) ->
  false.

apply_style(Tags, [ { Key, Style } | Rest ]) when erlang:is_list(Key) ->
  case proplists:is_defined(Key, Tags) of
    true ->
      Style;
    false ->
      apply_style(Tags, Rest)
  end;
apply_style(Tags, [ { {Key, Value}, Style } | Rest ]) ->
  case proplists:get_value(Key, Tags) of
    Value ->
      Style;
    _ ->
      apply_style(Tags, Rest)
  end;
apply_style(Tags, []) ->
  [].

analyze(Tag) ->
  Counted = db_dirty_fold(fun(#streets_way{ tags = Tags }, TagDict) ->
    case proplists:get_value(Tag, Tags) of
      undefined ->
        TagDict;
      Val ->
        orddict:update_counter(Val, 1, TagDict)
    end
  end, orddict:new(), streets_way),
  lists:sort(fun({_AK, CountA}, {_BK, CountB}) ->
    CountA > CountB
  end, Counted).

ways_to_file(Filename, ImageWidth, Predicate, HowToStyle) ->
  {ok, OutFile} = file:open(Filename, [write]),
  BBox = {MaxLat, MaxLon, MinLat, MinLon} =
    db_dirty_fold(fun(#streets_node{ coordinates = {Lat, Lon} }, {MaxLat, MaxLon, MinLat, MinLon}) ->
      {
        erlang:max(MaxLat, Lat),
        erlang:max(MaxLon, Lon),
        erlang:min(MinLat, Lat),
        erlang:min(MinLon, Lon)
      }
    end, {-90, -180, 90, 180}, streets_node),
  {Left, Top} = mercator_projection(MinLon, MaxLat),
  {Right, Bottom} = mercator_projection(MaxLon, MinLat),
  io:format("[to_file][bbox] ~p~n", [BBox]),
  io:format("[to_file][projected_bbox] ~p ~p~n", [{Left, Top}, {Right, Bottom}]),
  {Width, Height} = { Right - Left, Top - Bottom },
  ImageHeight = ImageWidth * (Height / Width),
  io:format(OutFile, "<svg width=\"~p\" height=\"~p\">~n", [ImageWidth, ImageHeight]),
  %% io:format(OutFile, "<rect x=\"0\" y=\"0\" width=\"~p\" height=\"~p\" />", [
  %%   ImageWidth, ImageHeight
  %% ]),
  db_dirty_fold(fun(Way = #streets_way{ nodes = NodeIds, tags = Tags }, {Printed, All}) ->
    case All rem 10000 of
      0 ->
        io:format("Printed ~p of ~p~n", [Printed, All]);
      _ ->
        ok
    end,
    case Predicate(Tags) of
      true ->
        Nodes = find_all(NodeIds, streets_node),
        %% io:format("[to_file][way] ~p~n", [ Way ]),
        %% io:format("[to_file][nodes] ~p~n", [ Nodes ]),
        SvgAttributes = get_attributes(HowToStyle(Tags)),
        SvgPath = get_path_from_nodes(Nodes, {Left, Top}, {Width, Height}, ImageWidth, ImageHeight),
        io:format(OutFile, "<path~s d=\"~s\" />~n", [ SvgAttributes, SvgPath ]),
        {Printed + 1, All + 1};
      false ->
        {Printed, All + 1}
    end
  end, {0, 0}, streets_way),
  io:format(OutFile, "</svg>", []).

get_attributes(Attributes) ->
  lists:foldl(fun({Key, Value}, Str) ->
    Str ++ io_lib:format(" ~s=\"~s\"", [Key, Value])
  end, "", Attributes).

get_path_from_nodes(Nodes, {Left, Top}, {Width, Height}, ImageWidth, ImageHeight) ->
  {_, Str} =
    lists:foldl(fun(#streets_node{ coordinates = {Lat, Lon} }, {IsFirst, Str}) ->
      {X, Y} = mercator_projection(Lon, Lat),
      {X0, Y0} = {X - Left, Top - Y},
      {PosX, PosY} = {ImageWidth * (X0 / Width), ImageHeight * (Y0 / Height)},
      Cmd = case IsFirst of true -> "M"; false -> "L" end,
      {false, Str ++ io_lib:format(" ~s ~p ~p", [Cmd, PosX, PosY])}
    end, {true, ""}, Nodes),
  Str.

find_all(Ids, Table) ->
  mnesia:activity(transaction, fun() ->
    lists:foldl(fun(Id, Results) ->
      case mnesia:read(Table, Id) of
        [ NewResult ] ->
          [ NewResult | Results ];
        _ ->
          Results
      end
    end, [], Ids)
  end).

mercator_projection(Lon, Lat) ->
  { deg_to_rad(Lon), math:log(math:tan(math:pi() / 4 + deg_to_rad(Lat) / 2)) }.

deg_to_rad(X) ->
  X * math:pi() / 180.

fill_db(Filename) ->
  Nodes = [ node() ],
  mnesia:create_schema(Nodes),
  mnesia:start(),
  mnesia:create_table(streets_node, [
    {attributes, record_info(fields, streets_node)},
    {disc_copies, Nodes},
    {type, set}
  ]),
  mnesia:create_table(streets_way, [
    {attributes, record_info(fields, streets_way)},
    {disc_copies, Nodes},
    {type, set}
  ]),
  mnesia:wait_for_tables([ streets_node, streets_way ], 5000),

  EventFun =
    fun(Event, State = #state{ count = Count, way_count = WayCount }) ->
      %% io:format("[fill_db][event] ~p~n", [ Event ]),
      case Event of
        {element, Element = #xml_stream_element{ name = "node" }} ->
          Node = create_node(Element), 
          io:format("[fill_db][node] ~p ~p~n", [ Count, Node ]),
          mnesia:activity(transaction, fun() ->
            mnesia:write(Node)
          end),
          State#state{
            count = Count + 1
          };
        {element, Element = #xml_stream_element{ name = "way" }} ->
          Way = create_way(Element),
            io:format("[fill_db][way] ~p ~p~n", [ WayCount, Way ]),
            mnesia:activity(transaction, fun() ->
              mnesia:write(Way)
            end),
            State#state{
              way_count = WayCount + 1
            };
        _ ->
          State
      end
    end,
  xml_stream:file(Filename, #xml_stream_options{
    keep_list = [ "node", "way" ],
    event_fun = EventFun,
    event_state = #state{}
  }).

db_dirty_fold(F, Initial, Table) ->
  db_dirty_fold(F, Initial, Table, mnesia:dirty_first(Table)).

db_dirty_fold(F, Acc, Table, Key) ->
  case Key of
    '$end_of_table' ->
      Acc;
    UsableKey ->
      [ Record ] = mnesia:dirty_read(Table, UsableKey),
      NextKey = mnesia:dirty_next(Table, UsableKey),
      db_dirty_fold(F, F(Record, Acc), Table, NextKey)
  end.

create_way(#xml_stream_element{ name = "way", attributes = Attributes, children = Children }) ->
  #streets_way{
    id = list_to_integer(proplists:get_value("id", Attributes)),
    nodes = [ create_nd(Child) || Child = #xml_stream_element{ name = "nd" } <- Children ],
    tags = [
      {proplists:get_value("k", ChildAttributes), proplists:get_value("v", ChildAttributes)}
    ||
      #xml_stream_element{ name = "tag", attributes = ChildAttributes } <- Children
    ]
  }.

create_nd(#xml_stream_element{ name = "nd", attributes = Attributes }) ->
  list_to_integer(proplists:get_value("ref", Attributes)).

create_node(#xml_stream_element{ name = "node", attributes = Attributes }) ->
  #streets_node{
    id = list_to_integer(proplists:get_value("id", Attributes)),
    coordinates = {
      list_to_float(proplists:get_value("lat", Attributes)),
      list_to_float(proplists:get_value("lon", Attributes))
    }
  }.
