-module(n2o_file).
-author('Andrii Zadorozhnii').
-include_lib("n2o/include/wf.hrl").
-include_lib("kernel/include/file.hrl").
-compile(export_all).

-define(ROOT, wf:config(n2o,upload,code:priv_dir(n2o))).
-define(NEXT, 256*1024). % 256K chunks for best 25MB/s speed
-define(STOP, 0).

% Callbacks

filename(#ftp{sid=Sid,filename=FileName}) -> filename:join(wf:to_list(Sid),FileName).

% N2O Protocols

info(#ftp{status={event,_}}=FTP, Req, State) ->
    wf:info(?MODULE,"Event Message: ~p",[FTP#ftp{data= <<>>}]),
    Module=State#cx.module,
    Reply=try Module:event(FTP)
          catch E:R -> Error=wf:stack(E,R), wf:error(?MODULE,"Catch: ~p:~p~n~p",Error), Error end,
    {reply,wf:format({io,n2o_nitrogen:render_actions(wf:actions()),Reply}),Req,State};

info(#ftp{id=Link,sid=Sid,filename=FileName,status= <<"init">>,block=Block,offset=Offset,size=TotalSize}=FTP,Req,State) ->
    Root=?ROOT,
    RelPath=(wf:config(n2o,filename,n2o_file)):filename(FTP),
    FilePath=filename:join(Root,RelPath),
    ok=filelib:ensure_dir(FilePath),
    FileSize=case file:read_file_info(FilePath) of {ok,Fi} -> Fi#file_info.size; {error,_} -> 0 end,

    wf:info(?MODULE,"Info Init: ~p Offset: ~p Block: ~p~n",[FilePath,FileSize,Block]),

    % Name={Sid,filename:basename(FileName)},
    Block2=case Block of 0 -> ?STOP; _ -> ?NEXT end,
    Offset2=case FileSize >= Offset of true -> FileSize; false -> 0 end,
    FTP2=FTP#ftp{block=Block2,offset=Offset2,filename=RelPath,data= <<>>},

    n2o_async:stop(file,Link),
    n2o_async:start(#handler{module=?MODULE,class=file,group=n2o,state=FTP2,name=Link}),

    {reply,wf:format(FTP2),Req,State};

info(#ftp{id=Link,sid=Sid,filename=FileName,status= <<"send">>}=FTP,Req,State) ->
    wf:info(?MODULE,"Info Send: ~p",[FTP#ftp{data= <<>>}]),
    Reply=try gen_server:call(n2o_async:pid({file,Link}),FTP)
        catch E:R -> wf:error(?MODULE,"Info Error call the sync: ~p~n",[FTP#ftp{data= <<>>}]),
            FTP#ftp{data= <<>>,block=?STOP} end,
    wf:info(?MODULE,"reply ~p",[Reply#ftp{data= <<>>}]),
    {reply,wf:format(Reply),Req,State};

info(#ftp{status= <<"recv">>}=FTP,Req,State) -> {reply,wf:format(FTP),Req,State};

info(#ftp{status= <<"relay">>}=FTP,Req,State) -> {reply,wf:format(FTP),Req, State};

info(Message,Req,State) -> wf:info(?MODULE, "Info Unknown message: ~p",[Message]),
    {unknown,Message,Req,State}.

% N2O Handlers

proc(init,#handler{state=#ftp{sid=Sid}=FTP}=Async) ->
    wf:info(?MODULE,"Proc Init: ~p",[FTP#ftp{data= <<>>}]),
    wf:send(Sid,FTP#ftp{data= <<>>,status={event,init}}),
    {ok,Async};

proc(#ftp{id=Link,sid=Sid,data=Data,filename=FileName,status= <<"send">>,block=Block}=FTP,
     #handler{state=#ftp{data=State,size=TotalSize,offset=Offset,filename=RelPath}}=Async) when Offset+Block >= TotalSize ->
	wf:info(?MODULE,"Proc Stop ~p, last piece size: ~p", [FTP#ftp{data= <<>>},byte_size(Data)]),
	case file:write_file(filename:join(?ROOT,RelPath),<<Data/binary>>,[append,raw]) of
		{error,Reason} -> {reply,{error,Reason},Async};
		ok ->
            FTP2=FTP#ftp{data= <<>>,block=?STOP},
            wf:send(Sid,FTP2#ftp{status={event,stop},filename=RelPath}),
			spawn(fun() -> n2o_async:stop(file,Link) end),
			{stop,normal,FTP2,Async#handler{state=FTP2}} end;

proc(#ftp{sid=Sid,data=Data,block=Block}=FTP,
     #handler{state=#ftp{data=State,offset=Offset,filename=RelPath}}=Async) ->
    FTP2=FTP#ftp{status= <<"send">>,offset=Offset+Block },
    wf:info(?MODULE,"Proc Process ~p",[FTP2#ftp{data= <<>>}]),
    case file:write_file(filename:join(?ROOT,RelPath),<<Data/binary>>,[append,raw]) of
        {error,Reason} -> {reply,{error,Reason},Async};
        ok -> {reply,FTP2#ftp{data= <<>>},Async#handler{state=FTP2#ftp{filename=RelPath}}} end.
