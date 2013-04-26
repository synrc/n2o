-module(action_set).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record) ->
    Target = Record#set.target,
    Value = wf:js_escape(wf:to_list(Record#set.value)),
    wf:f("$('~s').html(\"~s\");", [Target, Value]).

set(Element, Value) ->
    wf:wire(Element, #set { value=Value }).
