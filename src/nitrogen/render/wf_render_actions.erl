-module(wf_render_actions).
-author('Andrew Zadorozhny').
-include_lib ("n2o/include/wf.hrl").
-compile(export_all).

render_action(Action) ->
    Module = element(#action.module,Action),
    Res = Module:render_action(Action),
    case Res of
         Res when is_tuple(Res) -> render_action(Res);
         Str when is_list(Str) -> Str;
         A -> [] end.

