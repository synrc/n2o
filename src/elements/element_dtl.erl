-module(element_dtl).
-author('Maxim Sokhatsky').
-include("wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, dtl).

render_element(Record=#dtl{}) ->
    ModName = list_to_atom(Record#dtl.file ++ "_view"),
%    M = 
%     case code:ensure_loaded(ModName) of {module,Module} -> Module; _ -> 
    File = code:lib_dir(Record#dtl.app) ++ "/" ++ Record#dtl.folder ++ "/" ++ Record#dtl.file ++ "." ++Record#dtl.ext,
%    Module = ModName,
%    Options = [{out_dir,"."}],
%    erlydtl:compile(File, ModName, Options),
%    error_logger:info_msg("File ~p Module ~p Options ~p",[File, ModName, Options]),
    
%    ModName end,
    M = ModName,
    {ok,R} = M:render([{K,wf:render(V)} || {K,V} <- Record#dtl.bindings] ++ if Record#dtl.bind_script==true -> [{script,wf_context:script()}]; true-> [] end),
    R.
