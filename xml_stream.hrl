-record(xml_stream_options, {
  keep_list,
  event_fun,
  event_state
}).

-record(xml_stream_element, {
  name,
  attributes,
  children = []
}).
