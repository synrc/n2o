-module(element_dtl).
-author('Maxim Sokhatsky').
-include("wf.hrl").
-compile(export_all).

render_element(Record=#dtl{}) ->
    M = list_to_atom(wf:to_list(Record#dtl.file) ++ "_view"),
    File = case code:lib_dir(wf:to_atom(Record#dtl.app)) of
                {error,bad_name} -> wf:to_list(Record#dtl.app);
                A -> A end ++ "/" ++ wf:to_list(Record#dtl.folder)
         ++ "/" ++ wf:to_list(Record#dtl.file) ++ "." ++ wf:to_list(Record#dtl.ext),
    {ok,R} = M:render([{K,wf:render(V)} || {K,V} <- Record#dtl.bindings] ++ 
        if Record#dtl.bind_script==true -> [{script,wf:script()}]; true-> [] end),
    R.
