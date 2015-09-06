-module(n2o_file).
-author('Andrii Zadorozhnii').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

% N2O Protocols

info(#ftp{status="init"}=FTP, Req, #cx{}=State) ->
    wf:info(?MODULE,"File Transfer Init: ~p~n",[FTP]),
    application:set_env(n2o,formatter,bert),
    n2o_async:start(#handler{module=?MODULE,class=file,group=n2o,
                             name={FTP#ftp.sid,FTP#ftp.filename,FTP#ftp.hash}}),
    {reply,wf:format(FTP#ftp{source=wf:version()}),Req,State};

info(#ftp{sid=Sid,filename=File,hash=Hash,status="send"}=FTP, Req, State) ->
    wf:info(?MODULE,"File: ~p~n",[FTP]),
    Reply = try gen_server:call(n2o_async:pid({file,{Sid,File,Hash}}),FTP)
          catch E:R -> #ftp{data={E,R}} end,
    {reply,wf:format(Reply),Req,State};

info(Message, Req, State) -> {unknown,Message, Req, State}.

% N2O Handlers

proc(init,Async) ->
    {ok, Async};

proc(#ftp{}=FTP,#handler{}=Async) ->
    wf:info(?MODULE,"File Transfer Call: ~p~n",[FTP]),
    {reply,FTP,Async#handler{state=FTP}}.
