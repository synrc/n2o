-module(n2o_file).
-author('Andrii Zadorozhnii').
-include_lib("n2o/include/wf.hrl").
-include_lib("kernel/include/file.hrl").
-compile(export_all).

-define(ROOT, wf:config(n2o,upload,code:priv_dir(n2o))).
-define(next, 256*1024). % 256K chunks for best 22MB/s speed
-define(stop, 0).

% N2O Protocols

info(#ftp{sid=Sid,filename=Filename,hash=Hash,status= <<"init">>, offset=Size, block=B, data=Msg}=FTP, Req, State) ->
    application:set_env(n2o,formatter,bert),

    Dir   = lists:concat([?ROOT,'/',wf:to_list(Sid),'/']),
    File  = filename:join([Dir,Filename]),
    FSize = case file:read_file_info(File) of {ok, Fi} -> Fi#file_info.size; {error, _} -> 0 end,
    filelib:ensure_dir(Dir),

    wf:info(?MODULE, "File Transfer Init: ~p: Offset:~p Block: ~p~n",[File, FSize, B]),

    Name   = { Sid, Filename, Hash },
    Block  = case B of 0 -> ?stop; _ -> ?next end,
    Offset = case FSize >= Size of true -> FSize; false -> 0 end,
    F2     = FTP#ftp{block = Block, offset = Offset, data = <<>> },

    n2o_async:stop(file,Name),
    n2o_async:start(#handler{module=?MODULE,class=file,group=n2o, state=F2, name=Name}),

    {reply,wf:format(F2),Req,State};

info(#ftp{sid=Sid,filename=File,hash=Hash,status= <<"send">>}=FTP, Req, State) ->
    wf:info(?MODULE,"FTP:~p",[FTP#ftp {data = <<>> }] ),
    Reply = try gen_server:call(n2o_async:pid({file,{Sid,File,Hash}}),FTP)
          catch E:R -> wf:error(?MODULE, "error call the sync: ~p ~p", [E, R]),
                       FTP#ftp{data= wf:to_binary({E,R}), block=?stop} end,
    wf:info(?MODULE,"reply ~p", [Reply#ftp{data = <<>>}]),
    {reply,wf:format(Reply),Req, State};

info(Message, Req, State) -> {unknown,Message, Req, State}.

% N2O Handlers

proc(init,Async) -> {ok, Async};

proc(#ftp{sid=Sid, data=Msg, status= <<"send">>, block=B,filename=Filename}=FTP,
     #handler{state=#ftp{data=State,offset=Offset}}=Async) when erlang:byte_size(Msg) < B ->
    case file:write_file(filename:join([?ROOT,wf:to_list(Sid),Filename]), <<Msg/binary>>, [append,raw]) of
            ok -> {stop, normal, FTP#ftp{data= <<"">>,block=?stop}, Async#handler{state=FTP#ftp{block=?stop}}};
   {error, Rw} -> {reply, {error, Rw}, Async} end;

proc(#ftp{sid=Sid,data=Msg, block=Block, filename=Filename}=FTP,
     #handler{state=#ftp{data=State, offset=Offset}}=Async) ->
    F2 = FTP#ftp{status= <<"send">>, offset=Offset + Block },
    wf:info(?MODULE,"send ~p", [F2#ftp{data= <<"">>}]),
    case file:write_file(filename:join([?ROOT,wf:to_list(Sid),Filename]), <<Msg/binary>>, [append,raw]) of
            ok -> {reply, F2#ftp{data= <<"">>}, Async#handler{state=F2}};
   {error, Rw} -> {reply, {error, Rw}, Async} end.
