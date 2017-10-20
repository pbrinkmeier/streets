-module(xml_stream).
-export([
  file/2
]).
-include("xml_stream.hrl").

-record(state, {
  element_stack = [],
  cb_state
}).

file(Filename, #xml_stream_options{ keep_list = KeepList, event_fun = Cb, event_state = CbState }) ->
  EventFun = fun(Event, Location, State = #state{ element_stack = ElementStack, cb_state = CurrentCbState }) ->
    case Event of
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
            NewCbState = Cb({element, Completed}, CurrentCbState),
            State#state{
              element_stack = [],
              cb_state = NewCbState
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
    {event_state, #state{ cb_state = CbState }}
  ]).
