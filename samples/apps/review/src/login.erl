-module(login).
-compile(export_all).
-include_lib("kvs/include/feed.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/wf.hrl").

main() -> #dtl{file="login",app=review,bindings=[{body,body()},{folders,folders()}]}.
folders() -> string:join([filename:basename(F)||F<-filelib:wildcard(code:priv_dir(review)++"/snippets/*/")],",").

body() ->
 [ #span   { id=display },                #br{},
   #span   { body="Login: " },            #textbox{id=user,autofocus=true}, #br{},
   #span   { body="Join/Create Feed: " }, #textbox{id=pass},
   #button { id=loginButton, body="Login",postback=login,source=[user,pass]} ].

event(login) ->
    User = case wf:q(user) of <<>> -> "anonymous";
                              undefined -> "anonymous";
                              E -> wf:to_list(E) end,
    wf:user(User),
    wf:info(?MODULE,"User: ~p",[wf:user()]),
    wf:redirect("index.htm?room="++wf:to_list(wf:q(pass))),
    ok;

event(_) -> [].
