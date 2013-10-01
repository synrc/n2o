-module(action_update).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_action(Record) ->
    Type    = Record#update.type,
    Target  = Record#update.target,
    Elements = Record#update.elements,
    Html = case Elements of
                E when element(2,E) == is_element -> wf_core:render_item(Elements);
                E when element(2,E) == is_action  -> wf_core:render_item(Elements);
                E -> wf_core:render(E) end,
    wf:f("$('#~s').~s('~s');", [Target, atom_to_list(Type), Html]).

update(Target, Elements) -> update(html, Target, Elements).
replace(Target, Elements) -> update(replaceWith, Target, Elements).
insert_top(Target, Elements) -> update(prepend, Target, Elements).
insert_bottom(Target, Elements) -> update(append, Target, Elements).
insert_before(Target, Elements) -> update(before, Target, Elements).
insert_after(Target, Elements) -> update('after', Target, Elements).
remove(Target) -> update(remove, Target, []).

update(Type, Target, Elements) ->
    wf_context:add_action(#update{type=Type, target=Target, elements=Elements }).
