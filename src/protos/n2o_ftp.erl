-module(n2o_ftp).
-description('N2O File Protocol').
-include_lib("n2o/include/n2o.hrl").
-include_lib("kernel/include/file.hrl").
-export([info/3,proc/2,filename/1,root/0]).

-define(ROOT, filename:join(begin {ok, Cwd} = file:get_cwd(), Cwd end,
              application:get_env(n2o,upload,code:priv_dir(n2o)))).
-define(NEXT, 256*1024). % 256K chunks for best 25MB/s speed
-define(STOP, 0).

root() -> ?ROOT.

% Callbacks

filename(#ftp{sid=_Sid,filename=FileName}) -> FileName. %filename:join(lists:concat([Sid]),FileName).

% File Transfer Protocol

info(#ftp{status = {event, _}}=FTP, Req, State) ->
    {reply, {bert, n2o_nitro:io(FTP, State)}, Req, State};

info(#ftp{id = Link, status = <<"init">>, block = Block, offset = Offset}=FTP, Req, State) ->
    Root=?ROOT,
    RelPath=(application:get_env(n2o,filename,n2o_ftp)):filename(FTP),
    FilePath = filename:join(Root, RelPath),
    ok = filelib:ensure_dir(FilePath),
    FileSize = case file:read_file_info(FilePath) of
        {ok, Fi} -> Fi#file_info.size;
        {error, _} -> 0
    end,

%    ?LOG_INFO("FTP INFO INIT: ~p",[ FTP#ftp{data = <<>>, sid = <<>>} ]),

    Block2 = case Block of 0 -> ?STOP; _ -> ?NEXT end,
    Offset2 = case FileSize >= Offset of true -> FileSize; false -> 0 end,
    FTP2 = FTP#ftp{block = Block2, offset = Offset2, data = <<>>, filename=FilePath},

    catch n2o_pi:stop(file, Link),
    n2o_pi:start(#pi{module=?MODULE, table=file, sup=n2o, state=FTP2, name=Link}),

    {reply, {bert, FTP2}, Req, State};

info(#ftp{id = Link, status = <<"send">>}=FTP, Req, State) ->
%    ?LOG_INFO("FTP SEND: ~p", [FTP#ftp{data = <<>>, sid = <<>>}]),
    Reply = try
        n2o_pi:send(file, Link, FTP)
    catch E:R ->
        ?LOG_ERROR(#{error => E, reason => R, loc => ftpinfo}),
        FTP#ftp{data = <<>>,sid = <<>>, block = ?STOP}
    end,
    {reply, {bert, Reply}, Req, State};

info(Message, Req, State) -> {unknown, Message, Req, State}.

% n2o Handlers

proc(init, #pi{}=Async) ->
    {ok, Async};

proc(#ftp{sid = Token, data = Data, status = <<"send">>, block = Block, meta = ClientId} = FTP,
     #pi{name = Link, state = #ftp{size = TotalSize, offset = Offset, filename = RelPath}} = Async)
     when Offset + Block >= TotalSize ->
%        ?LOG_INFO("FTP PROC FINALE: ~p~n", [ Link ]),
        case file:write_file(filename:join(?ROOT,RelPath), <<Data/binary>>, [append,raw]) of
            {error, Reason} ->
%                ?LOG_ERROR("WRITE ERROR: ~p~n", [ filename:join(?ROOT, RelPath) ]),
                {reply, {error, Reason}, Async};
            ok ->
                FTP2 = FTP#ftp{data = <<>>, sid = <<>>,offset = TotalSize, block = ?STOP},
                FTP3 = FTP2#ftp{status = {event, stop}, filename = RelPath},
                spawn(fun() -> catch n2o_ring:send(ws,{publish, <<"events/1//index/anon/",ClientId/binary,"/",Token/binary>>,
                                        term_to_binary(FTP3)}),
                               Sid = case n2o:depickle(Token) of {{S,_},_} -> S; X -> X end,
 %                              ?LOG_INFO("NOTIFY SEND TO WEB: ~p~n", [ {Sid, FTP3} ]),
                               catch n2o:send(Sid,{direct,FTP3}) end),
                spawn(fun() -> n2o_pi:stop(file, Link) end),
                {stop, normal, FTP2, Async#pi{state = FTP2}}
        end;

proc(#ftp{data = Data, block = Block} = FTP,
    #pi{state = #ftp{offset = Offset, filename = RelPath}}=Async) ->
    FTP2 = FTP#ftp{status = <<"send">>, offset = Offset + Block },
    case file:write_file(filename:join(?ROOT, RelPath), <<Data/binary>>, [append,raw]) of
        {error, Reason} -> {reply, {error, Reason}, Async};
        ok -> {reply, FTP2#ftp{data = <<>>},
                      Async#pi{state = FTP2#ftp{data = <<>>, filename = RelPath}}} end;

proc(_,Async) -> {reply, #ftpack{}, Async}.
