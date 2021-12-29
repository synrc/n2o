-module(n2o_multipart).
-export([init/2,terminate/3]).

% curl -F file=@./Image.tiff http://localhost:50111/multipart

acc_multipart(Req0, Acc) ->
    case cowboy_req:read_part(Req0) of
        {ok, Headers, Req1} -> {ok, Body, Req} = stream_body(Req1, <<>>), acc_multipart(Req, [{Headers,Body}|Acc]);
        {done, Req} -> {ok, Acc, Req}
    end.

stream_body(Req0, Acc) ->
    case cowboy_req:read_part_body(Req0) of
        {more, Data, Req} -> stream_body(Req, << Acc/binary, Data/binary >>);
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req}
    end.

init(Req, Opts) ->
    {ok, Body, Req2} = acc_multipart(Req, []),
    {Headers, Data} = hd(Body),
    {file, _, Filename, ContentType} = cow_multipart:form_data(Headers),
    GUID = erp:guid(),
    Size = erlang:integer_to_list(erlang:size(Data)),
    file:write_file(GUID, Data, [raw, binary]),
    Req3 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>, <<"guid">> => GUID, <<"size">> => Size },
                                 io_lib:format("File ~s uploaded with size ~s.~n",[GUID,Size]), Req2),
    {ok, Req3, Opts}.

terminate(_Reason, _Req, _State) ->
    ok.
