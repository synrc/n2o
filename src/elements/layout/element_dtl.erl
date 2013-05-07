-module (element_dtl).
-include_lib ("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, dtl).

render_element(Record) ->
    File = wf:to_list(Record#dtl.file),
    render_template(code:priv_dir(web) ++ "/templates/" ++ File, File, Record#dtl.bindings).

render_template(FullPathToFile,ViewFile,Data) ->
    Pieces = string:tokens(ViewFile,"/"),
    Name = string:join(Pieces,"_"),
    error_logger:info_msg("File: ~p", Name),
    Name1 = filename:basename(Name,".html"),
    ModName = list_to_atom(Name1 ++ "_view"),
    error_logger:info_msg("Module: ~p", ModName),
    erlydtl:compile(FullPathToFile,ModName),
    {ok,Render} = ModName:render(Data),
    error_logger:info_msg("DTL: ~p", Render),
    iolist_to_binary(Render).
