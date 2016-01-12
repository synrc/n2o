-module(n2o_static).
-description('N2O Static Bridge to files in LING image, MAD bundle or OS').
-author('Maxim Sokhatsky').
-compile(export_all).
-include_lib("kernel/include/file.hrl").

init(_, _, _) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, {dir, Path, Extra}) when is_binary(Path) -> rest_init(Req, {dir, binary_to_list(Path), Extra});
rest_init(Req, {dir, Path, Extra}) ->
	{PathInfo, Req2} = cowboy_req:path_info(Req),
	Info = {ok, #file_info{type=regular,size=0}},
	FileName = filename:join([Path|PathInfo]),
	wf:info(?MODULE,"Rest Init: ~p~n\r",[FileName]),
    {ok, Req2, {FileName, Info, Extra}}.

malformed_request(Req, State) -> {State =:= error, Req, State}.

forbidden(Req, State={_, {ok, #file_info{type=directory}}, _}) -> {true, Req, State};
forbidden(Req, State={_, {error, eacces}, _}) -> {true, Req, State};
forbidden(Req, State={_, {ok, #file_info{access=Access}}, _}) when Access =:= write; Access =:= none -> {true, Req, State};
forbidden(Req, State) -> {false, Req, State}.

content_types_provided(Req, State={Path, _, Extra}) ->
    wf:info(?MODULE,"Content Type Provided: ~p~n\r",[Path]),
	case lists:keyfind(mimetypes, 1, Extra) of
		false -> {[{cow_mimetypes:web(Path), get_file}], Req, State};
		{mimetypes, Module, Function} -> {[{Module:Function(Path), get_file}], Req, State};
		{mimetypes, Type} -> {[{Type, get_file}], Req, State}
	end.

resource_exists(Req, State={_, {ok, #file_info{type=regular}}, _}) -> {true, Req, State};
resource_exists(Req, State) -> {false, Req, State}.

generate_etag(Req, State={Path, {ok, #file_info{size=Size, mtime=Mtime}}, Extra}) ->
	case lists:keyfind(etag, 1, Extra) of
		false -> {generate_default_etag(Size, Mtime), Req, State};
		{etag, Module, Function} -> {Module:Function(Path, Size, Mtime), Req, State};
		{etag, false} -> {undefined, Req, State}
	end.

generate_default_etag(Size, Mtime) ->
	{strong, list_to_binary(integer_to_list(
		erlang:phash2({Size, Mtime}, 16#ffffffff)))}.

last_modified(Req, State={_, {ok, #file_info{mtime=Modified}}, _}) -> {Modified, Req, State}.

get_file(Req, State={Path, {ok, #file_info{size=_Size}}, _}) ->
    StringPath = wf:to_list(unicode:characters_to_binary(Path,utf8,utf8)),
    [_Type,Name|RestPath]=SplitPath = filename:split(StringPath),
    wf:info(?MODULE,"Split Path: ~p~n\r",[SplitPath]),
    %wf:info(?MODULE,"Code Path: ~p~n\r",[filename:join([code:lib_dir(Name)|RestPath])]),
	FileName = filename:absname(StringPath),
    wf:info(?MODULE,"Abs Name: ~p~n\r",[FileName]),
    Raw = case file:read_file(FileName) of
    {ok,Bin} -> Bin;
    {error,_} ->
        case mad_repl:load_file(StringPath) of
        {ok,ETSFile} -> ETSFile;
        {error,_} ->
            case file:read_file(filename:join([code:lib_dir(Name)|RestPath])) of
            {ok,ReleaseFile} -> ReleaseFile;
            {error,_} -> <<>> end end end,
    wf:info(?MODULE,"Cowboy Requested Static File: ~p~n\r ~p~n\r",[Raw,filename:absname(StringPath)]),
	Sendfile = fun (Socket, Transport) ->
		case Transport:send(Socket, Raw) of
			{ok, _} -> ok;
			{error, closed} -> ok;
			{error, etimedout} -> ok;
			_ -> ok end end,
	{{stream, size(Raw), Sendfile}, Req, State}.
