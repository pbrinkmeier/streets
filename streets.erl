-module(streets).
-export([
  fill_db/1
]).
-include("streets.hrl").
-include("xml_stream.hrl").

-record(state, {
  count = 0,
  way_count = 0
}).

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
