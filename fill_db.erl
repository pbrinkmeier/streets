-module(fill_db).
-export([
  run/1
]).
-include("streets.hrl").
-include("xml_stream.hrl").

-record(state, {
  count = 0
}).

run(Filename) ->
  EventFun =
    fun(Event, State) ->
      case Event of
        X ->
          io:format("[fill_db][event] ~p~n", [Event]),
          State
      end
    end,
  xml_stream:file(Filename, #xml_stream_options{
    keep_list = [ "node" ],
    event_fun = EventFun,
    event_state = #state{}
  }).
