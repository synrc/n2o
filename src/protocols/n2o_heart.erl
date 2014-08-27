-module(n2o_heart).
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

info(<<"PING">> = Ping, Req, State) ->
%    wf:info(?MODULE,"Ping Message: ~p",[Ping]),
    {reply, wf:json([]), Req, State};

info(<<"N2O,",Rest/binary>>=InitMarker,Req,State) ->
%    wf:info(?MODULE,"Ping Message: ~p",[Ping]),
    {reply, wf:json([]), Req, State};

info(<<"N2O,",Rest/binary>> = InitMarker, Req, State) ->
    wf:info(?MODULE,"N2O INIT: ~p",[Rest]),
    Module = State#context.module,
    InitActions = case Rest of
         <<>> -> Elements = try Module:main() catch X:Y -> wf:error_page(X,Y) end,
                 wf_core:render(Elements),
                 [];
          Binary -> Pid = wf:depickle(Binary), % FIXME TODO Legacy Nitrogen Compatible Code should be more pretty
                    X = Pid ! {'N2O',self()},
                    R = receive A -> n2o_nitrogen:render_actions(A) after 100 ->
                        QS = element(14, Req),
                        wf:redirect(case QS of <<>> -> ""; _ -> "?" ++ wf:to_list(QS) end),
                        []
                    end,
                    R end,
    try Module:event(init) catch C:E -> wf:error_page(C,E) end,
    Actions = wf:render(get(actions)),
    {reply, wf:json([{eval,iolist_to_binary([InitActions,Actions])}]), Req, State}.
