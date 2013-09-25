-module(n2o_cowboy_rest).
-export([init/3, rest_init/2, resource_exists/2, allowed_methods/2, content_types_provided/2, to_html/2,
         content_types_accepted/2, handle_urlencoded_data/2, delete_resource/2]).

-record(st, {resource_module = undefined :: atom(),
             resource_id = undefined :: binary()}).

init(_, _, _) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
    {Resource, Req1} = cowboy_req:binding(resource, Req),
    Module = case rest_module(Resource) of
                 {ok, M} -> M;
                 _       -> undefined
             end,
    {Id, Req2} = cowboy_req:binding(id, Req1),
    {ok, Req2, #st{resource_module = Module, resource_id = Id}}.


resource_exists(Req, #st{resource_module = undefined} = State)       -> {false, Req, State};
resource_exists(Req, #st{resource_id     = undefined} = State)       -> {true, Req, State};
resource_exists(Req, #st{resource_module = M, resource_id = Id} = S) -> {M:exists(Id), Req, S}.

allowed_methods(Req, #st{resource_id = undefined} = State) -> {[<<"GET">>, <<"POST">>], Req, State};
allowed_methods(Req, State)                                -> {[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

%% GET
content_types_provided(Req, State) ->
    {[
      {<<"text/html">>, to_html},
      {<<"application/json">>, to_json}
     ], Req, State}.


to_html(Req, #st{resource_module = M, resource_id = Id} = State) ->
    Body = case Id of
               undefined -> [M:to_html(Resource) || Resource <- M:get()];
               _         -> M:to_html(M:get(Id))
           end,
    Html = case erlang:function_exported(M, html_layout, 2) of
               true -> M:html_layout(Req, Body);
               false -> default_html_layout(Body)
           end,
    {Html, Req, State}.

%% POST, PUT
content_types_accepted(Req, State) ->
    {[
      {<<"application/x-www-form-urlencoded">>, handle_urlencoded_data}
     ], Req, State}.

handle_urlencoded_data(Req, #st{resource_id = undefined} = State) ->
    {false, Req, State};

handle_urlencoded_data(Req, #st{resource_module = M, resource_id = Id} = State) ->
    {ok, Data, Req2} = cowboy_req:body_qs(Req),
    M:put(Id, Data),
    {true, Req2, State}.


%% DELETE
delete_resource(Req,  #st{resource_module = M, resource_id = Id} = State) ->
    M:delete(Id),
    {true, Req, State}.


%% private
default_html_layout(Body) ->
    [<<"<html><body>">>, Body, <<"</body></html>">>].


rest_module(Module) when is_binary(Module) -> rest_module(binary_to_list(Module));
rest_module(Module) ->
 try
     M = list_to_existing_atom(Module),
     M:module_info(),
     true = M:is_rest(),
     {ok, M}
 catch
     error:Error -> {error, Error}
 end.
