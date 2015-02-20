-module(login).
-compile(export_all).
-include_lib("kvs/include/feed.hrl").
-include_lib("n2o/include/wf.hrl").

main() -> #dtl{file="login",app=review,bindings=[{body,body()},{folders,folders()}]}.
folders() -> string:join([filename:basename(F)||F<-filelib:wildcard(code:priv_dir(review)++"/snippets/*/")],",").

body() ->
 [ #span   { id=display },                #br{},
   #span   { body="Login: " },            #textbox{id=user,autofocus=true}, #br{},
   #span   { body="Join/Create Feed: " }, #textbox{id=pass},
   #button { body="Login",postback=login,source=[user,pass]}].

event(init) ->
    [ index:event({client,{"feed",element(2,F#feed.id)}}) || F <-kvs:all(feed)],
    n2o_session:ensure_sid([],?CTX,[]);

event(login) ->
    User = case wf:q(user) of [] -> "anonymous";
                              undefined -> "anonymous";
                              E -> E end,
    wf:user(User),
    wf:info(?MODULE,"User: ~p",[wf:user()]),
    wf:redirect("/index?room="++wf:to_list(wf:q(pass))),
    ok;

event(_) -> [].
