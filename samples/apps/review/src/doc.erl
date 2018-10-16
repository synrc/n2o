-module(doc).
-compile(export_all).
-include_lib("kvs/include/feed.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/wf.hrl").

main() -> #dtl{file="doc",app=review,bindings=[{body,body()}]}.

body() -> case wf:user() of
               undefined -> wf:user("anonymous");
                       _ -> skip end,
  [ #h2      { body = "Docs search" },
    #textbox { id   = query },
    #button  { body = "Search", postback=search, source=[query] },
    #panel   { id   = results } ].

event({client,Panel}) -> wf:insert_top(results,Panel);
event(search)         -> wf:update(results,#panel{id=results}),
                         Pid = self(), Query = wf:q(query), spawn(fun() -> search(Pid,Query) end), ok;
event(_)              -> [].

sections(Path,Match,Pid)  ->
    Page=filename:basename(Path),
    App = lists:nth(4,lists:reverse(filename:split(Path))),
    Forms=#panel{body=[
          #h5{body=filename:join([App,filename:basename(Page,".htm")])}, [ begin
              Url=["index.htm?code=",
                   wf:pickle(iolist_to_binary([wf:to_binary(App),"/doc/web/",Page,$#,Sec]))],
              #panel{body=#link{body=T,href=Url}}
          end||[Sec,T] <- Match]]},
    Pid ! {client,Forms}.

re(Q) -> <<"(?:<h\\d[^>]*?\\ id=[\'\"](sec\\d{1,3})[\'\"][^>]*?>(.*?)<\\/h\\d>.*?)+",Q/binary,"[^>]+<">>.
search(Pid,Q) ->
    lists:map(fun(Path) -> spawn(fun() ->
        {ok,Bin}=file:read_file(Path),
        case re:run(Bin,re(Q),[unicode,global,{capture,[1,2],binary},dotall,caseless]) of
            {match,Match} -> sections(Path,Match,Pid);
            nomatch -> [] end end) end,
    filelib:wildcard(application:get_env(n2o,search,"/var/www/sites/synrc.space/apps/*/doc/web/*.htm"))).
