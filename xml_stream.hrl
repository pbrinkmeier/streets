-record(xml_stream_options, {
  keep_list
}).

-record(xml_stream_element, {
  name,
  attributes,
  children = []
}).
