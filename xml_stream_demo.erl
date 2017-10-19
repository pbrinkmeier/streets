-module(xml_stream_demo).
-export([
  run/0
]).
-include("xml_stream.hrl").

run() ->
  Filename = "/home/paul/OsmData/karlsruhe.osm",

  xml_stream:start_link(Filename, erlang:self(), #xml_stream_options{
    keep_list = [ "node" ]
  }),
  receive_parser_messages().

receive_parser_messages() ->
  receive
    done ->
      io:format("[client] done~n");
    Message ->
      io:format("[client][message] ~p~n", [Message]),
      receive_parser_messages()
  end.
