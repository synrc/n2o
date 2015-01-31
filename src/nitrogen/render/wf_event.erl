-module(wf_event).
-author('Maxim Sokhatsky').
-include_lib ("n2o/include/wf.hrl").
-compile(export_all).

new(bin,Data) ->
    wf:f("ws.send(enc(tuple(atom('bin'),bin('~s'))));",[wf:pickle(Data)]).

new(undefined, _, _, _, _, _) -> [];
new(Postback, Element, Delegate, Name, Data, Source) ->
    Module = wf:coalesce([Delegate, ?CTX#cx.module]),
    Sources = "[" ++ string:join([ wf:f("'~s'",[X]) || X<- Source ],",") ++ "]",
    Event = #ev{name=Name, module=Module, msg=Postback, trigger=Element},
    wf:f("{ if (validateSources(~s)) ws.send(enc(tuple(atom('~w'),bin('~s'),bin('~s'),~s)));"
                               "else console.log('Validation Error'); }",
        [Sources,wf:config(n2o,event,pickle),Element,wf:pickle(Event),Data]).
