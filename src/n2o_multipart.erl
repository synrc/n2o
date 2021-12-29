-module(n2o_multipart).
-export([init/2,terminate/3]).

% test multipart
% curl -F file=@/Users/5HT/Desktop/40.tiff http://localhost:8000/multipart

acc_multipart(Req0, Acc) ->
    case cowboy_req:read_part(Req0) of
        {ok, Headers, Req1} -> {ok, Body, Req} = stream_body(Req1, <<>>), acc_multipart(Req, [{Headers,Body}|Acc]);
        {done, Req} -> {ok, Acc, Req}
    end.

stream_body(Req0, Acc) ->
    case cowboy_req:read_part_body(Req0) of
        {more, Data, Req} ->
            io:format("Received chunk ~p.~n", [erlang:size(Data)]),
            stream_body(Req, << Acc/binary, Data/binary >>);
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req}
    end.

init(Req, Opts) ->
    {ok, Body, Req2} = acc_multipart(Req, []),
    {Headers, Data} = hd(Body),
    {file, <<"file">>, Filename, ContentType} = cow_multipart:form_data(Headers),
    io:format("Received file ~p of content-type ~p size ~p.~n", [Filename, ContentType, erlang:size(Data)]),
    {ok, Req2, Opts}.

terminate(_Reason, _Req, _State) ->
    ok.
