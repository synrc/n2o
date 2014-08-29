-module(n2o_heart).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

info({text,<<"PING">> = Ping}=Message, Req, State) ->
    wf:info(?MODULE,"PING: ~p",[Message]),
    {reply, wf:json([]), Req, State};
info({text,<<"N2O,",Rest/binary>> = InitMarker}=Message, Req, State) ->
    wf:info(?MODULE,"N2O INIT: ~p",[Message]),
    Module = State#context.module,
    InitActions = case Rest of
         <<>> -> Elements = try Module:main() catch X:Y -> wf:error_page(X,Y) end,
                 wf_render:render(Elements),
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
    {reply, wf:json([{eval,iolist_to_binary([InitActions,Actions])}]), Req, State};

info(Message, Req, State) -> {unknown,Message, Req, State}.
