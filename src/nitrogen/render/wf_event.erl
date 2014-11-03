-module(wf_event).
-author('Maxim Sokhatsky').
-include_lib ("n2o/include/wf.hrl").
-compile(export_all).

new(bin,Data) ->
    wf:f("ws.send(enc(tuple(atom('bin'),bin('~s'))));",[wf:pickle(Data)]).

new(undefined, _, _, _, _, _) -> [];
new(Postback, Element, Delegate, Name, Data, Source) ->
    Module = wf:coalesce([Delegate, ?CTX#cx.module]),
    {Detail,SourceName} = case Source of
                 [N] -> {wf:f("querySourceRaw('~s')",[N]),wf:to_list(N)};
                   _ -> {"''",wf:to_list(Source)} end,
    Event = #ev{name=Name, module=Module, msg=Postback, trigger=Element},
    wf:f("{var  event=new CustomEvent('validation'), name='~s';"
               "event.initCustomEvent('validation',true,true,~s);"
         "if (document.getElementById(name).dispatchEvent(event)) "
               "ws.send(enc(tuple(atom('~w'),bin(name),bin('~s'),~s)));"
         "else console.log('Validation failed of source ~s');"
         "}",
        [Element,Detail,wf:config(n2o,event,pickle),wf:pickle(Event),Data,SourceName]).
