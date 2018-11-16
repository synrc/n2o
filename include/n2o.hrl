-ifndef(N2O_HRL).
-define(N2O_HRL, true).

-record(handler, { name, module, class, group, config, state, seq}).
-record(cx,      { handlers=[], actions=[], req=[], module=[], lang=[], path=[],
                   session=[], formatter=bert, params=[], node=[], client_pid=[], state=[], from=[], vsn = [] }).

-define(CTX(ClientId), n2o:cache(ClientId)).
-define(REQ(ClientId), (n2o:cache(ClientId))#cx.req).

% API

-define(QUERING_API, [init/2, finish/2]).
-define(SESSION_API, [init/2, finish/2, get_value/2, set_value/2, clear/0]).
-define(MESSAGE_API, [send/2, reg/1, reg/2, unreg/1]).

-define(N2O_JSON, (application:get_env(n2o,json,jsone))).

% IO protocol

-record(bin,     { data=[] }).
-record(client,  { data=[] }).
-record(server,  { data=[] }).

% Nitrogen Protocol

-record(init,    { token=[] }).
-record(pickle,  { source=[], pickled=[], args=[] }).
-record(flush,   { data=[] }).
-record(direct,  { data=[] }).
-record(ev,      { module=[], msg=[], trigger=[], name=[] }).

% File Transfer Protocol

-record(ftp,     { id=[], sid=[], filename=[], meta=[], size=[], offset=[], block=[], data=[], status=[] }).
-record(ftpack,  { id=[], sid=[], filename=[], meta=[], size=[], offset=[], block=[], data=[], status=[] }).

-endif.
