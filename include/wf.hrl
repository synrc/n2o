-ifndef(N2O_HRL).
-define(N2O_HRL, true).

-record(handler, { name, module, class, group, config, state}).
-record(cx,      { handlers, actions, req, module, lang, path, session, formatter=false, params, form, state=[] }).

-define(CTX, (get(context))).
-define(REQ, (get(context))#cx.req).

% API

-define(HANDLER_API, [init/2, finish/2]).
-define(FAULTER_API, [error_page/2]).
-define(ROUTING_API, [init/2, finish/2]).
-define(QUERING_API, [init/2, finish/2]).
-define(SESSION_API, [init/2, finish/2, get_value/2, set_value/2, clear/0]).
-define(PICKLES_API, [pickle/1, depickle/1]).
-define(MESSAGE_API, [send/2, reg/1, reg/2, unreg/1]).

% IO protocol

-record(io,      { eval, data }).
-record(bin,     { data }).

% Client/Server protocol

-record(client,  { data }).
-record(server,  { data }).

% Nitrogen Protocol

-record(pickle,  { source, pickled, args }).
-record(flush,   { data }).
-record(direct,  { data }).
-record(ev,      { module, msg, trigger, name }).

% File Transfer Protocol

-record(ftp,     { id, sid, filename, meta, size, offset, block, data, status }).

% HTTP

-record(http,     { url, method, body, headers = [] }).

-endif.
