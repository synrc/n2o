-module(n2o_cowboy_rest).
-author('Dmitry Bushmelev').
-record(st, {resource_module = undefined :: atom(), resource_id = undefined :: binary()}).
-export([init/3, rest_init/2, resource_exists/2, allowed_methods/2, content_types_provided/2,
         to_html/2, content_types_accepted/2, handle_urlencoded_data/2, delete_resource/2]).

init(_, _, _) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {Resource, Req1} = cowboy_req:binding(resource, Req),
    Module = case rest_module(Resource) of {ok, M} -> M; _ -> undefined end,
    {Id, Req2} = cowboy_req:binding(id, Req1),
    %% temporary solution, in near future may changed back to binary
    ResId = case Id of undefined -> undefined; Binary -> binary_to_list(Binary) end,
    {ok, Req2, #st{resource_module = Module, resource_id = ResId}}.

resource_exists(Req, #st{resource_module = undefined} = State)       -> {false, Req, State};
resource_exists(Req, #st{resource_id     = undefined} = State)       -> {true, Req, State};
resource_exists(Req, #st{resource_module = M, resource_id = Id} = S) -> {M:exists(Id), Req, S}.

allowed_methods(Req, #st{resource_id = undefined} = State) -> {[<<"GET">>, <<"POST">>], Req, State};
allowed_methods(Req, State)                                -> {[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) -> {[{<<"text/html">>, to_html}], Req, State}.

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

content_types_accepted(Req, State) -> {[{<<"application/x-www-form-urlencoded">>, handle_urlencoded_data}], Req, State}.

handle_urlencoded_data(Req, #st{resource_module = M, resource_id = Id} = State) ->
    {ok, Binary, Req2} = cowboy_req:body_qs(Req),
    Data = format_data(Binary),
    Processable = case erlang:function_exported(M, data_processable, 2) of
                      true  -> M:data_processable(Id, Data);
                      false -> true
                  end,
    {case Processable of
         true  -> case Id of
                      undefined -> M:post(Data);
                      _         -> M:put(Id, Data)
                  end;
         false -> false
     end, Req2, State}.

delete_resource(Req,  #st{resource_module = M, resource_id = Id} = State) ->
    M:delete(Id),
    {true, Req, State}.

default_html_layout(Body) -> [<<"<html><body>">>, Body, <<"</body></html>">>].
format_data(Data) -> [{binary_to_list(Key), binary_to_list(Value)} || {Key, Value} <- Data].

rest_module(Module) when is_binary(Module) -> rest_module(binary_to_list(Module));
rest_module(Module) ->
    try M = list_to_existing_atom(Module),
        M:module_info(),
        true = M:is_rest(),
        {ok, M}
    catch error:Error -> {error, Error} end.
