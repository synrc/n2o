-module(login).
-compile(export_all).
-include_lib("kvs/include/feed.hrl").
-include_lib("n2o/include/wf.hrl").

main() -> #dtl{file = "login", app=n2o_sample,bindings=[{body,body()}]}.

body() ->
 [ #span{id=display}, #br{},
            #span{body="Login: "}, #textbox{id=user,autofocus=true}, #br{},
            #span{body="Room: "}, #textbox{id=pass},
            #button{body="Login",postback=login,source=[user,pass]} ].

event(init) -> 
    [ index:event({client,{"feed",wf:f("~p",[F#feed.id])}}) || F <-kvs:all(feed) ],
    js_session:ensure_sid([],?CTX);

event(login) ->
    User = case wf:q(user) of [] -> "anonymous"; E -> E end,
    wf:user(User),
    wf:redirect("/index?room="++wf:to_list(wf:q(pass))),
    ok;

event(_) -> [].
