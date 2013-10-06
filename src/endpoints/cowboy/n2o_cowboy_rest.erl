-module(n2o_cowboy_rest).
-author('Dmitry Bushmelev').
-record(st, {resource_module = undefined :: atom(), resource_id = undefined :: binary()}).
-export([init/3, rest_init/2, resource_exists/2, allowed_methods/2, content_types_provided/2,
         to_html/2, to_json/2, content_types_accepted/2, delete_resource/2,
         handle_urlencoded_data/2, handle_json_data/2]).

init(_, _, _) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {Resource, Req1} = cowboy_req:binding(resource, Req),
    Module = case rest_module(Resource) of {ok, M} -> M; _ -> undefined end,
    {Id, Req2} = cowboy_req:binding(id, Req1),
    {ok, Req2, #st{resource_module = Module, resource_id = Id}}.

resource_exists(Req, #st{resource_module = undefined} = State)       -> {false, Req, State};
resource_exists(Req, #st{resource_id     = undefined} = State)       -> {true, Req, State};
resource_exists(Req, #st{resource_module = M, resource_id = Id} = S) -> {M:exists(Id), Req, S}.

allowed_methods(Req, #st{resource_id = undefined} = State) -> {[<<"GET">>, <<"POST">>], Req, State};
allowed_methods(Req, State)                                -> {[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) -> {[{<<"text/html">>, to_html}, {<<"application/json">>, to_json}], Req, State}.

to_html(Req, #st{resource_module = M, resource_id = Id} = State) ->
    Body = case Id of
               undefined -> [M:to_html(Resource) || Resource <- M:get()];
               _         -> M:to_html(M:get(Id))
           end,
    Html = case erlang:function_exported(M, html_layout, 2) of
               true  -> M:html_layout(Req, Body);
               false -> default_html_layout(Body)
           end,
    {Html, Req, State}.

default_html_layout(Body) -> [<<"<html><body>">>, Body, <<"</body></html>">>].

to_json(Req, #st{resource_module = M, resource_id = Id} = State) ->
    Struct = case Id of
                 undefined -> {struct, [{M, [{struct, M:to_json(Resource)} || Resource <- M:get()]}]};
                 _         -> {struct, M:to_json(M:get(Id))}
             end,
    {iolist_to_binary(n2o_json:encode(Struct)), Req, State}.

content_types_accepted(Req, State) -> {[{<<"application/x-www-form-urlencoded">>, handle_urlencoded_data},
                                        {<<"application/json">>, handle_json_data}], Req, State}.

handle_urlencoded_data(Req, #st{resource_module = M, resource_id = Id} = State) ->
    {ok, Data, Req2} = cowboy_req:body_qs(Req),
    {handle_data(M, Id, Data), Req2, State}.

handle_json_data(Req, #st{resource_module = M, resource_id = Id} = State) ->
    {ok, Binary, Req2} = cowboy_req:body(Req),
    Data = case n2o_json:decode(Binary) of {struct, Struct} -> Struct; _ -> [] end,
    {handle_data(M, Id, Data), Req2, State}.

handle_data(Mod, Id, Data) ->
    Valid = case erlang:function_exported(Mod, validate, 2) of
                true  -> Mod:validate(Id, Data);
                false -> default_validate(Mod, Id, Data)
            end,
    case Valid of
        true  -> case Id of undefined -> Mod:post(Data); _ -> put(Mod,Id,Data) end;
        false -> false
    end.

put(Mod,Id, Data) ->
    Object = Mod:get(Id),
    Filled = Mod:fill(Data,Object),
    case element(2,Object) =/= element(2,Filled) of
        true  -> Mod:delete(element(2,Object));
        false -> true end,
    Mod:post(Data).

default_validate(Mod, Id, Data) ->
    Allowed = case erlang:function_exported(Mod, keys_allowed, 1) of
                  true  -> Mod:keys_allowed(proplists:get_keys(Data));
                  false -> true
              end,
    validate_match(Mod, Id, Allowed, proplists:get_value(<<"id">>, Data)).

validate_match(_Mod, undefined, true, undefined)                  -> false;
validate_match(_Mod, undefined, true, NewId) when NewId == <<"">> -> false;
validate_match( Mod, undefined, true, NewId)                      -> not Mod:exists(NewId);
validate_match(_Mod,       _Id, true, undefined)                  -> true;
validate_match( Mod,        Id, true, NewId) when Id =/= NewId    -> not Mod:exists(NewId);
validate_match(   _,         _,    _, _)                          -> false.

delete_resource(Req,  #st{resource_module = M, resource_id = Id} = State) -> {M:delete(Id), Req, State}.

rest_module(Module) when is_binary(Module) -> rest_module(binary_to_list(Module));
rest_module(Module) ->
    try M = list_to_existing_atom(Module),
        M:module_info(),
        true = M:is_rest(),
        {ok, M}
    catch error:Error -> {error, Error} end.
