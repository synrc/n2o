% vim: sw=4 ts=4 et ft=erlang
% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (http_basic_auth_security_handler).
-behaviour (security_handler).
-export ([
    init/2, 
    finish/2,
    main/0
]).

-author('tobbe@tornkvist.org').

%% @doc
%%
%% Basic usage:
%% 
%% In the module that dispatches a request to Nitrogen, add a call to
%% nitrogen:handler(http_basic_auth_security_handler, CallbackMod) 
%% after nitrogen:init_request/2 but before nitrogen:run/0. 
%%
%% This will tell Nitrogen to use the http_basic_auth_security_handler
%% security handler.
%%
%% CallbackMod is an Erlang module that exports the following functions:
%%
%% -spec realm() -> string().    
%%
%% realm/0 - simply return the authentication realm as a string.
%%
%% 
%% -spec is_authenticated(atom(), string()) -> bool().
%%
%% is_authenticated/2 - makes it possible to e.g timeout a session and thus
%% insisting on authentication being performed. If the function returns 'false',
%% the authenticate/2 callback will be called in succession; else no further
%% action will be taken.
%%
%%
%% -spec authenticate(atom(), string(), string()) -> bool().
%%
%% authenticate/3 - will decide if authentication will be requested by the client.
%% In case it returns 'true', no authentication will be requested.

%% -spect is_protected(atom()) -> bool().
%%
%% is_protected/1 - will decide if a resource is protected by authentication.
%% In case it returns 'true', authentication will be requested over the
%% resource.
%%
%% EXAMPLES:
%%
%% realm() -> "visitor".
%%
%% is_protected(index) -> true;
%% is_protected(_) -> false;
%% is_authenticated(Module, User) -> false.
%%
%% authenticate(web_list, X, X)       -> true;
%% authenticate(web_list, _, _)       -> false;
%% authenticate(Module, User, Password) -> true.
%%
%% @end


%% Attempt authentication. The general pattern below
%% is to try each step of decoding the authorization string.
%% If the step is successful, then move on to the step below.
%% If the step is NOT successful, then prompt for authentication.

init(CallbackMod, State) ->
    PageModule = wf:page_module(),
    case CallbackMod:is_protected(PageModule) of
	true ->
	    case wf:header(authorization) of
		AuthHeader when AuthHeader /= undefined ->           
		    check_auth_header(PageModule, CallbackMod, AuthHeader);
		_ -> 
		    prompt_for_authentication(CallbackMod)
	    end;
	_ ->
	    do_nothing
    end,
    {ok, State}.

finish(_Config, State) -> 
    {ok, State}.


check_auth_header(Module, CallbackMod, AuthHeader) ->
    case string:tokens(AuthHeader, " ") of
        ["Basic", Digest] ->
            decode_digest(Module, CallbackMod, Digest);
        _ ->
            prompt_for_authentication(CallbackMod)
    end.

decode_digest(Module, CallbackMod, Digest) ->
    case string:tokens(base64:decode_to_string(Digest), ":") of
        [User, Password] ->
            check_is_authenticated(Module, CallbackMod, User, Password);
        _ ->
            prompt_for_authentication(CallbackMod)
    end.

check_is_authenticated(Module, CallbackMod, User, Password) ->
    case CallbackMod:is_authenticated(Module, User) of
        false ->
            authenticate_user(Module, CallbackMod, User, Password);
        _ ->
            ok
    end. 

authenticate_user(Module, CallbackMod, User, Password) ->
    case CallbackMod:authenticate(Module, User, Password) of
        true  -> 
            ok;
        _ ->
            prompt_for_authentication(CallbackMod)
    end.

prompt_for_authentication(CallbackMod) ->
    % Set the callback module, then set the page module to 
    % http_basic_auth_security_handler.erl, which will cause
    % the page to be rendered using the main() function below.
    wf:state(callback_mod, CallbackMod),
    wf_context:page_module(?MODULE).

main() ->
    CallbackMod = wf:state(callback_mod),
    Realm = CallbackMod:realm(),
    wf:header("WWW-Authenticate", "Basic realm=\""++Realm++"\""),
    wf:status_code(401),
    "<strong>Authentication required!</strong>".
