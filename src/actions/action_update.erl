-module(action_update).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record) ->
    Type    = Record#update.type,
    Target  = Record#update.target,
    Elements = Record#update.elements,
    {ok, Html} = wf_render_elements:render_elements(Elements),
    wf:f("$('~s').~s('~s');", [Target, atom_to_list(Type), wf:js_escape(Html)]).

update(Target, Elements) -> update(html, Target, Elements).
replace(Target, Elements) -> update(replaceWith, Target, Elements).
insert_top(Target, Elements) -> update(prepend, Target, Elements).
insert_bottom(Target, Elements) -> update(append, Target, Elements).
insert_before(Target, Elements) -> update(before, Target, Elements).
insert_after(Target, Elements) -> update('after', Target, Elements).
remove(Target) -> update(remove, Target, []).

update(Type, Target, Elements) -> wf_context:add_action(#update{type=Type, target=Target, elements=Elements }).
