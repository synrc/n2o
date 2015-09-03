-module(n2o_file).
-author('Andrii Zadoorozhnii').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

info(#ftp{}=FTP, Req, State) ->
    wf:info(?MODULE,"File Transfer: ~p~n",[FTP]),
    {reply,wf:format(FTP#ftp{source=wf:version()}),Req,State};

info(Message, Req, State) -> {unknown,Message, Req, State}.
