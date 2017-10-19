-module(xml_stream).
-export([
  start_link/3,
  file/3
]).
-include("xml_stream.hrl").

-record(state, {
  element_stack = []
}).

start_link(Filename, Receiver, Options) ->
  erlang:spawn_link(?MODULE, file, [Filename, Receiver, Options]).

file(Filename, Receiver, #xml_stream_options{ keep_list = KeepList }) ->
  EventFun = fun(Event, Location, State = #state{ element_stack = ElementStack }) ->
    case Event of
      endDocument ->
        Receiver ! done,
        State;
      {startElement, _Uri, ElementName, _QualName, Attributes} ->
        case erlang:length(ElementStack) > 0 orelse lists:member(ElementName, KeepList) of
          true ->
            BetterAttributes = [ {Key, Value} || {_AUri, _Prefix, Key, Value} <- Attributes ],
            State#state{
              element_stack = [ #xml_stream_element{ name = ElementName, attributes = BetterAttributes } | ElementStack ]
            };
          false ->
            State
        end;
      {endElement, _Uri, ElementName, _QualName} ->
        case ElementStack of
          [] ->
            State;
          [ Completed = #xml_stream_element{ name = ElementName } ] ->
            Receiver ! {element, Completed},
            State#state{
              element_stack = []
            };
          [ Completed = #xml_stream_element{ name = ElementName }, Parent = #xml_stream_element{ children = Children } | Rest ] ->
            State#state{
              element_stack = [ Parent#xml_stream_element{ children = [ Completed | Children ] } | Rest ]
            }
        end;
      Unknown ->
        %% io:format("[parser][unknown] ~p~n", [Unknown]),
        State
    end
  end,
  xmerl_sax_parser:file(Filename, [
    {event_fun, EventFun},
    {event_state, #state{}}
  ]).
