-module(n2o_rest).
-author('Loïc Hoguin').

-export([init/3]).
-export([rest_init/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([content_types_provided/2]).
%-export([charsets_provided/2]).
-export([resource_exists/2]).
-export([content_types_accepted/2]).
-export([is_conflict/2]).
-export([generate_etag/2]).
-export([collection_to_html/2]).
-export([collection_to_json/2]).
-export([userdata_to_json/2]).
-export([userdata_to_html/2]).
-export([userdata_from_form/2]).
-export([delete_resource/2]).
-export([delete_completed/2]).

-record(state, {auth = undefined, collection = undefined :: boolean(), userid = undefined}).

init(_, _, _) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
    State = case lists:keyfind(auth, 1, Opts) of
        {auth, AuthOpts} -> #state{auth=AuthOpts};
        false -> #state{} end,
    {Bucket, Req1} = cowboy_req:binding(bucket, Req),
    {Item, Req2} = cowboy_req:binding(key, Req1),
    {Key, Req3} = cowboy_req:path_info(Req2),
    error_logger:info_msg("1 Bucket: ~p, Item: ~p, Key: ~p", [Bucket,Item,Key]),
    if Item =:= undefined -> {ok, Req3, State#state{collection=true}};
        Item =/= undefined -> {ok, Req3, State#state{collection=false}} end.

allowed_methods(Req, State=#state{collection=true}) -> {[<<"GET">>], Req, State};
allowed_methods(Req, State=#state{collection=false}) -> {[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

is_authorized(Req, State=#state{auth=undefined}) -> {true, Req, State};
is_authorized(Req, State=#state{auth=AuthOpts}) ->
    case n2o_auth:authenticate(Req, AuthOpts) of
        {false, Req2} -> {{false, n2o_auth:methods(AuthOpts)}, Req2, State};
        {UserID, Req2} -> {true, Req2, State#state{userid=UserID}} end.

content_types_provided(Req, State=#state{collection=true}) -> {[ {<<"text/html">>, collection_to_html}, {<<"application/json">>, collection_to_json} ], Req, State};
content_types_provided(Req, State=#state{collection=false}) -> {[ {<<"text/html">>, userdata_to_html}, {<<"application/json">>, userdata_to_json} ], Req, State}.

%charsets_provided(Req, State) -> {[{<<"utf-8">>, 1000}, {<<"*">>, 0}], Req, State}.

resource_exists(Req, State=#state{collection=true}) -> {true, Req, State};
resource_exists(Req, State=#state{collection=false}) ->
    {Bucket, Req2} = cowboy_req:binding(bucket, Req),
    {Item, Req3} = cowboy_req:binding(key, Req2),
    {Key, Req4} = cowboy_req:path_info(Req3),
    Module = list_to_atom(binary_to_list(Bucket)),
    Argument = binary_to_list(Item),
    Res = Module:exists(Argument),
    error_logger:info_msg("2 Module: ~p, Arg: ~p, Res: ~p", [Module,Argument,Res]),
    {Module:exists(Argument), Req4, State}.

content_types_accepted(Req, State=#state{collection=false}) ->
	{[
		{{<<"application">>,<<"x-www-form-urlencoded">>,[{<<"charset">>,<<"UTF-8">>}]},
			userdata_from_form},
		%% @todo Need normalize CTA for this one
		{<<"application/json">>, template_from_json}
	], Req, State}.

is_conflict(Req, State) ->
	case cowboy_req:qs_val(<<"new">>, Req) of
		{undefined, Req2} -> {false, Req2, State};
		{_, Req2} ->
			{Bucket, Req3} = cowboy_req:binding(bucket, Req2),
			{Item, Req4} = cowboy_req:binding(key, Req3),
			{Key, Req5} = cowboy_req:path_info(Req4),
    Module = list_to_atom(binary_to_list(Bucket)),
    Argument = binary_to_list(Item),
    Res = Module:exists(Argument),
    error_logger:info_msg("2 Module: ~p, Arg: ~p, Res: ~p", [Module,Argument,Res]),
			{Module:exists(Argument), Req5, State}
	end.

generate_etag(Req, State=#state{collection=true}) -> {undefined, Req, State};
generate_etag(Req, State=#state{collection=false}) -> {undefined, Req, State}.

collection_to_html(Req, State) ->
    {Bucket, Req1} = cowboy_req:binding(bucket, Req),
    Module = list_to_atom(binary_to_list(Bucket)),
    Items = Module:get([]),
    Body = [ <<"<table>">>, [ Module:to_html(User) || User <- Items], <<"</table>">>],
    {Body, Req1, State}.

collection_to_json(Req, State) -> Body = <<"{}">>, {Body, Req, State}.

userdata_to_html(Req, State) ->
    {Bucket, Req2} = cowboy_req:binding(bucket, Req),
    {Item, Req3} = cowboy_req:binding(key, Req2),
    {Key, Req4} = cowboy_req:path_info(Req3),
    Module = list_to_atom(binary_to_list(Bucket)),
    Argument = binary_to_list(Item),
    [Data] = Module:get(Argument),
    Body = Module:to_html(Data),
    {Body, Req4, State}.

userdata_to_json(Req, State) -> Body = <<"{}">>, {Body, Req, State}.

userdata_from_form(Req, State=#state{userid=UserID}) ->
	{Bucket, Req2} = cowboy_req:binding(bucket, Req),
	{Key, Req3} = cowboy_req:path_info(Req2),
	{ok, Values, Req4} = cowboy_req:body_qs(infinity, Req3),
	{<<"userdata">>, UserData} = lists:keyfind(<<"userdata">>, 1, Values),
	{<<"comments">>, Comments} = lists:keyfind(<<"comments">>, 1, Values),
	case fw_userdata_server:set_data(Bucket, Key,
			UserID, UserData, Comments) of
		ok ->
			{true, Req4, State};
		{error, Reason} ->
			io:format("put error ~s~n", [Reason]),
			Req5 = cowboy_req:set_resp_body(
				jsx:encode([{userdata, erlang:atom_to_binary(Reason, latin1)}]),
				Req4),
			{false, Req5, State}
	end.

delete_resource(Req, State) -> io:format("delete_resource ~p~n", [Req]), {true, Req, State}.
delete_completed(Req, State) -> {true, Req, State}.
