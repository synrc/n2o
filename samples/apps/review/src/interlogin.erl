-module(interlogin).
-compile(export_all).
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/user.hrl").
-include_lib("avz/include/avz.hrl").
-include_lib("kvs/include/user.hrl").

-define(LOGIN, [facebook, twitter, google, github, microsoft]).

main() ->
 avz:callbacks(?LOGIN),
 #dtl{file="dev",app=review,bindings=[{title,<<"Login">>},{body,body()},{folders,folders()}]}.
folders() -> string:join([filename:basename(F)||F<-filelib:wildcard(code:priv_dir(review)++"/snippets/*/")],",").

body() ->
  header() ++
  [#panel{class=main, body=[
    #panel{class=section, body=[
      #panel{class=[content,center], body=[
        #span{ id=display },
        #link{href="/", body=[#image{src="/static/S.svg",style="margin-top:0px;"}]},
        #h1{body= <<"Sign In/Up">>},
        #h2{body= <<"Select provider to enter through">>},
        #br{},
        #panel{class=["pure-button-group"], role=group, body=[
          avz:buttons(?LOGIN)
        ]}
      ]}
    ]},
    footer()
  ]} | avz:sdk(?LOGIN)].

event(init) ->
[wf:wire(#jq{target=Id, method=["classList.add"], args=["'pure-button','avz-button'"]}) 
  || Id <- [loginfb,twlogin,github_btn,microsoftbtn]],
wf:wire(#jq{target=gloginbtn, method=["classList.add"], args=["'google-button'"]});

event({register, U=#user{}}) ->
  case kvs:add(U#user{id=kvs:next_id("user",1)}) of 
    {ok, U1} -> avz:login_user(U1);
    {error,E} -> event({login_failed,E}) end;
  
event({login, #user{}=U, N}) -> 
  Updated = avz:merge(U,N), 
  ok = kvs:put(Updated), 
  avz:login_user(Updated);

event({login_failed, E}) -> 
  wf:update(display, #span{id=display, body=[E] });

event(X) -> wf:info(?MODULE,"Event:~p~n",[X]),avz:event(X).
api_event(X,Y,Z) -> avz:api_event(X,Y,Z).

header() -> 
  [#header{class=[header], body=[
    #panel{class=["menu", "pure-menu", "pure-menu-horizontal","pure-menu-fixed"], body=[
      #link{class=["pure-menu-heading"], href= <<"/">>, body=[#image{src="/static/S.svg"}, <<"Home">>]},

      #list{class=["pure-menu-list"], body=[
        case wf:user() of 
          undefined -> [
            #li{class=["pure-menu-item", "pure-menu-selected"], 
              body=[#link{class=["pure-menu-link"], body= <<"Sign In">>, url= <<"/login.html">>}]} ];
          User ->
            Email = wf:to_list(User#user.email),
            Avatar = case User#user.images of [] -> "holder.js/50x50"; [{_,A}|_] -> A end,
            [
              #li{class=["pure-menu-item"], body=[#link{class=["pure-menu-link"], body= [<<"Profile">>], url= <<"/profile.html">>}]},
              #li{class=["pure-menu-item"], body=[
                #link{class=["pure-menu-link"], body= ["Sign Out"], postback=logout, delegate=index} ]},
              #li{class=["pure-menu-item"], body=[Email]},
              #li{class=["pure-menu-item"], body=[
                #link{class=["pure-menu-link", "profile-pic"], body=[
                  #image{class=["pure-img"], image = Avatar} ]} ]}
            ]
        end
      ]}
    ]}
  ]}].

% 
footer() -> #footer{class=[footer], body=[ <<"&copy; 2017">> ]}.
