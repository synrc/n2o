-module(n2o_dynalo).
-compile(export_all).

-export([init/3]).
-export([rest_init/2]).
-export([malformed_request/2]).
-export([forbidden/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).
-export([last_modified/2]).
-export([generate_etag/2]).
-export([get_file/2]).

-type extra_etag() :: {etag, module(), function()} | {etag, false}.
-type extra_mimetypes() :: {mimetypes, module(), function()}
	| {mimetypes, binary() | {binary(), binary(), [{binary(), binary()}]}}.
-type extra() :: [extra_etag() | extra_mimetypes()].
-type opts() :: {file | dir, string() | binary()}
	| {file | dir, string() | binary(), extra()}
	| {priv_file | priv_dir, atom(), string() | binary()}
	| {priv_file | priv_dir, atom(), string() | binary(), extra()}.
-export_type([opts/0]).

-include_lib("kernel/include/file.hrl").

-type state() :: {binary(), {ok, #file_info{}} | {error, atom()}, extra()}.

init(_, _, _) ->
	{upgrade, protocol, cowboy_rest}.

%% @doc Resolve the file that will be sent and get its file information.
%% If the handler is configured to manage a directory, check that the
%% requested file is inside the configured directory.

-spec rest_init(Req, opts())
	-> {ok, Req, error | state()}
	when Req::cowboy_req:req().
rest_init(Req, {Name, Path}) ->
	rest_init_opts(Req, {Name, Path, []});
rest_init(Req, {Name, App, Path})
		when Name =:= priv_file; Name =:= priv_dir ->
	rest_init_opts(Req, {Name, App, Path, []});
rest_init(Req, Opts) ->
	rest_init_opts(Req, Opts).

rest_init_opts(Req, {priv_file, App, Path, Extra}) ->
	rest_init_info(Req, absname(priv_path(App, Path)), Extra);
rest_init_opts(Req, {file, Path, Extra}) ->
	rest_init_info(Req, absname(Path), Extra);
rest_init_opts(Req, {priv_dir, App, Path, Extra}) ->
	rest_init_dir(Req, priv_path(App, Path), Extra);
rest_init_opts(Req, {dir, Path, Extra}) ->
	rest_init_dir(Req, Path, Extra).

priv_path(App=n2o_sample, Path) -> priv_path(App, Path, "apps/");
priv_path(App, Path) -> priv_path(App, Path, "deps/").

priv_path(App, Path, Prefix) ->
    LApp = Prefix ++ atom_to_list(App) ++ "/priv",
	case LApp of
		PrivDir when is_list(Path) ->
			PrivDir ++ "/" ++ Path;
		PrivDir when is_binary(Path) ->
			<< (list_to_binary(PrivDir))/binary, $/, Path/binary >>
	end.

absname(Path) when is_list(Path) ->
	filename:absname(list_to_binary(Path));
absname(Path) when is_binary(Path) ->
	filename:absname(Path).

rest_init_dir(Req, Path, Extra) when is_list(Path) ->
	rest_init_dir(Req, list_to_binary(Path), Extra);
rest_init_dir(Req, Path, Extra) ->
	Dir = fullpath(Path), %filename:absname(Path)),
	{PathInfo, Req2} = cowboy_req:path_info(Req),
	Filepath = filename:join([Dir|PathInfo]),
	Len = byte_size(Dir),
	case fullpath(Filepath) of
		<< Dir:Len/binary, $/, _/binary >> ->
			rest_init_info(Req2, Filepath, Extra);
		_ ->
			{ok, Req2, error}
	end.

fullpath(Path) ->
	fullpath(filename:split(Path), []).
fullpath([], Acc) ->
	filename:join(lists:reverse(Acc));
fullpath([<<".">>|Tail], Acc) ->
	fullpath(Tail, Acc);
fullpath([<<"..">>|Tail], Acc=[_]) ->
	fullpath(Tail, Acc);
fullpath([<<"..">>|Tail], [_|Acc]) ->
	fullpath(Tail, Acc);
fullpath([Segment|Tail], Acc) ->
	fullpath(Tail, [Segment|Acc]).

rest_init_info(Req, Path, Extra) ->
%    io:format("File Requested: ~p ~n\r",[Path]),
	Info = {ok, #file_info{type=regular,size=0}},
	 %file:read_file_info(Path, [{time, universal}]),
	{ok, Req, {Path, Info, Extra}}.


%% @doc Reject requests that tried to access a file outside
%% the target directory.

-spec malformed_request(Req, State)
	-> {boolean(), Req, State}.
malformed_request(Req, State) ->
	{State =:= error, Req, State}.

%% @doc Directories, files that can't be accessed at all and
%% files with no read flag are forbidden.

-spec forbidden(Req, State)
	-> {boolean(), Req, State}
	when State::state().
forbidden(Req, State={_, {ok, #file_info{type=directory}}, _}) ->
	{true, Req, State};
forbidden(Req, State={_, {error, eacces}, _}) ->
	{true, Req, State};
forbidden(Req, State={_, {ok, #file_info{access=Access}}, _})
		when Access =:= write; Access =:= none ->
	{true, Req, State};
forbidden(Req, State) ->
	{false, Req, State}.

%% @doc Detect the mimetype of the file.

-spec content_types_provided(Req, State)
	-> {[{binary(), get_file}], Req, State}
	when State::state().
content_types_provided(Req, State={Path, _, Extra}) ->
	case lists:keyfind(mimetypes, 1, Extra) of
		false ->
			{[{cow_mimetypes:web(Path), get_file}], Req, State};
		{mimetypes, Module, Function} ->
			{[{Module:Function(Path), get_file}], Req, State};
		{mimetypes, Type} ->
			{[{Type, get_file}], Req, State}
	end.

%% @doc Assume the resource doesn't exist if it's not a regular file.

-spec resource_exists(Req, State)
	-> {boolean(), Req, State}
	when State::state().
resource_exists(Req, State={_, {ok, #file_info{type=regular}}, _}) ->
	{true, Req, State};
resource_exists(Req, State) ->
	{false, Req, State}.

%% @doc Generate an etag for the file.

-spec generate_etag(Req, State)
	-> {{strong | weak, binary()}, Req, State}
	when State::state().
generate_etag(Req, State={Path, {ok, #file_info{size=Size, mtime=Mtime}},
		Extra}) ->
	case lists:keyfind(etag, 1, Extra) of
		false ->
			{generate_default_etag(Size, Mtime), Req, State};
		{etag, Module, Function} ->
			{Module:Function(Path, Size, Mtime), Req, State};
		{etag, false} ->
			{undefined, Req, State}
	end.

generate_default_etag(Size, Mtime) ->
	{strong, list_to_binary(integer_to_list(
		erlang:phash2({Size, Mtime}, 16#ffffffff)))}.

%% @doc Return the time of last modification of the file.

-spec last_modified(Req, State)
	-> {calendar:datetime(), Req, State}
	when State::state().
last_modified(Req, State={_, {ok, #file_info{mtime=Modified}}, _}) ->
	{Modified, Req, State}.

%% @doc Stream the file.
%% @todo Export cowboy_req:resp_body_fun()?

-spec get_file(Req, State)
	-> {{stream, non_neg_integer(), fun()}, Req, State}
	when State::state().
get_file(Req, State={Path, {ok, #file_info{size=Size}}, _}) ->
    StringPath = binary_to_list(Path),
	FileName = absname(StringPath),
    Raw = case file:read_file(FileName) of
         {ok,Bin} -> Bin;
         {error,_} -> mad_repl:load_file(StringPath) end,
%    io:format("Cowboy Requested Static File: ~p~n\r ~p~n\r",[Raw,absname(StringPath)]),
	Sendfile = fun (Socket, Transport) ->
		case Transport:send(Socket, Raw) of
			{ok, _} -> ok;
			{error, closed} -> ok;
			{error, etimedout} -> ok;
			_ -> ok
		end
	end,
	{{stream, size(Raw), Sendfile}, Req, State}.
