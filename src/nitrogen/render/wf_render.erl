-module(wf_render).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

render_item(E) when element(2,E) == element -> wf_render_elements:render_element(E);
render_item(E) when element(2,E) == action  -> wf_render_actions:render_action(E);
render_item(E) -> E.
render(<<E/binary>>) -> E;
render(undefined) -> [];
render(Elements) when is_list(Elements) -> [ render_item(E) || E <- lists:flatten(Elements) ];
render(Elements) -> render_item(Elements).
