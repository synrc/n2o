-ifndef(N2O_HRL).
-define(N2O_HRL, true).

-define(CTX, (wf_context:context())).
-define(REQ, (wf_context:context())#cx.req).

-define(ACTION(Module), ancestor=action, trigger, target, module=Module, actions, source=[]).

% Actions
-record(wire,    {?ACTION(action_wire)}).
-record(event,   {?ACTION(action_event)}).
-record(jq,      {?ACTION(action_jq), property, method, args=[], right, format="~s"}).

-define(HANDLER_API, [init/2, finish/2]).
-define(FAULTER_API, [error_page/2]).
-define(ROUTING_API, [init/2, finish/2]).
-define(QUERING_API, [init/2, finish/2]).
-define(SESSION_API, [init/2, finish/2, get_value/2, set_value/2, clear/0]).
-define(PICKLES_API, [pickle/1, depickle/1]).
-define(MESSAGE_API, [send/2, reg/1, reg/2, unreg/1]).

-record(handler, {name, module, config, state}).
-record(cx,      {handlers, actions, req, module, lang, path, session, params, form, state=[]}).
-record(ev,      {module, msg, trigger, name :: api_event | control_event | event | atom() }).

%Binary messaging to browser
-record(binary, {
    id = 0      :: integer(),   % 4 bytes unsigned
    type = 0    :: integer(),   % 1 byte unsigned
    app = 0     :: integer(),   % 1 byte unsigned
    version = 0 :: integer(),   % 1 byte unsigned
    from = 0    :: integer(),   % 4 bytes unsigned
    to = 0      :: integer(),   % 4 bytes unsigned
    user1 = 0   :: integer(),   % 8 bytes signed float, user defined, e.g.: -define(TIMESTAMP, user1).
    user2 = 0   :: integer(),   % 8 bytes signed float, user defined, e.g.: -define(EXPIRES, user2).
    meta = <<>> :: binary(),    % binary
    data = <<>> :: binary() }). % binary
-endif.
