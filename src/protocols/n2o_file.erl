-module(n2o_file).
-author('Andrey Martemyanov').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

info(#ftp{}=FTP, Req, State) ->
    {reply,{binary,FTP},Req,State};

info(Message, Req, State) -> {unknown,Message, Req, State}.
