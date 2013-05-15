-module(element_dtl).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

reflect() -> record_info(fields, dtl).

render_element(Record) ->
    File = wf:to_list(Record#dtl.file),
    render_template(code:lib_dir(Record#dtl.app) ++ "/" ++ Record#dtl.folder ++ "/" ++ File, File,
                    Record#dtl.bindings ++ [{script,get(script)}]).

render_template(FullPathToFile,ViewFile,Data) ->
    Pieces = string:tokens(ViewFile,"/"),
    Name = string:join(Pieces,"_"),
    Name1 = filename:basename(Name,".html"),
    ModName = list_to_atom(Name1 ++ "_view"),
    M = case code:ensure_loaded(ModName) of
         {module,Module} -> Module;
         _ -> erlydtl:compile(FullPathToFile,ModName), ModName
         end,
    {ok,Render} = M:render(Data),
%    error_logger:info_msg("DTL: ~p", [Render]),
    iolist_to_binary(Render).
