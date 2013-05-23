-module(element_dtl).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, dtl).

render_element(Record) ->
    ModName = list_to_atom(Record#dtl.file ++ "_view"),
%    M = 
%     case code:ensure_loaded(ModName) of {module,Module} -> Module; _ -> 
    erlydtl:compile(code:lib_dir(Record#dtl.app) ++ "/" ++ Record#dtl.folder ++ "/" ++ Record#dtl.file ++ ".html",ModName),
%    ModName end,
    M = ModName,
    {ok,R} = M:render([{K,wf:render(V)} || {K,V} <- Record#dtl.bindings] ++ [{script,wf_context:script()}]),
    R.
