N2O: Erlang Web Framework on WebSockets
=======================================

Nitrogen 2 Optimized
--------------------

Information for Nitrogen users:

* Page construction from binaries
* Do all Actions through WebSocket channel
* Work within Cowboy processes
* Bert/jQuery only w/o JSON, urlencode, and nitrogen.js
* Compatible enough to transfer Nitrogen sites
* Clean codebase
* Separate Advanced Nitrogen elements
* GProc process registry
* Proper id and class attributes

WebSockets transport
--------------------

Nitrogen 2 Optimized, N2O was started as first Erlang Web Framework
that fully relies on WebSocket transport. Great compatibility with
Nitrogen was retained and many improvements was made like binary
page construction, binary data transfer, all events wire over
WebSocket channel, minumum process spawns, works within Cowboy
processes. Page render in N2O is several times faster than in
original Nitrogen.

Binary events over WebSockets
-----------------------------

N2O doesn’t use JSON, all message data passed over websockets
encoded with native Erlang External Term Format which is easily
parsed in JavaScript with Bert.decode(msg) and helps to avoid
complexity on server-side. Please refer to http://bert-rpc.org
for more information.

Optimized for speed
-------------------

Original Nitrogen was tested in production under high-load and
we decided to drop out nprocreg process registry along with
action_comet heavy process creation. N2O now creates only
one process for async websocket handler, all async operations
are handled withing Cowboy processes.

Why Erlang in Web?
------------------

We’ve measured all existing modern web frameworks with latest
functional languages and Cowboy still the king. You can see
raw HTTP performance of functional and C-like languages with
concurrent primitives (Go, D and Rust) on VAIO Z notebook
with i7640M processor:

![WebServers](http://synrc.com/lj/webcompare/connections.png)

We outperform full Nitrogen stack with only 2X downgrade
of raw HTTP Cowboy performance thus rise rendering performance
several times in compare to any other functional web framework
and for sure it is faster than raw HTTP node.js performance.

Sending HTML5 over wire?
------------------------

N2O do that. However we agree that most advanced technology
is to send only domain data over network (JSON or Binary) like
you do with Chaplin/CoffeScript and Meteor/JavaScript.
However Meteor is Node-based. And with “everything on client”
model would not be possible to prototype services easily
with all your Erlang infrastructure — you should prototype
a protocol always along with client.

So in case you system is built aroung Erlang infrastructure,
N2O is best choice you could made for fast prototyping,
easy of use, maintanance of codebase, etc. Despite HTML5
is tranfered over wire, you will have at hands access to
all your erlang services directly.

Templates vs DSL
----------------

We choose Nitrogen for simple and elegant way of typed HTML
page construction like in Scala Lift, OCaml Ocsigen and
Haskell Happstack. Templated based approach pushes to to
deal with raw HTML, like Yesod, ASP, PHP, JSP, Rails, Yaws,
ChicagoBoss. N2O goes further and optimize rendering from
binary iolists instead of slow Erlang lists originated by Nitrogen.

Main quality of N2O is fast prototyping. We also use it
in large scale projects. You can see complete chat example
working through WebSockets:

    -module(chat).
    -compile(export_all).
    -include_lib("n2o/include/wf.hrl").
    
    main() -> #template { file= code:priv_dir(web) ++ "/templates/index.html" }.
    title() -> <<"N2O">>.
    
    body() -> %% area of http handler
    {ok,Pid} = wf:comet(fun() -> chat_loop() end),
      [ #span { text= <<"Your chatroom name: ">> }, 
        #textbox { id=userName, text= <<"Anonymous">> },
        #panel { id=chatHistory, class=chat_history },
        #textbox { id=message },
        #button { id=sendButton, text= <<"Send">>, postback={chat,Pid}, source=[userName,message] },
        #panel { id=status } ].
    
    event({chat,Pid}) -> %% area of websocket handler
        Username = wf:q(userName),
        Message = wf:q(message),
        Terms = [ #span { text="Message sent" }, #br{} ],
        wf:insert_bottom(chatHistory, Terms),
        wf:wire("$('#message').focus(); $('#message').select(); "),
        wf:reg(room),
        Pid ! {message, Username, Message};
    
    event(Event) -> error_logger:info_msg("Event: ~p", [Event]).
    
    chat_loop() -> %% background worker ala comet
        receive 
            {message, Username, Message} ->
                Terms = [ #span { text=Username }, ": ",
                          #span { text=Message }, #br{} ],
                wf:insert_bottom(chatHistory, Terms),
                wf:wire("$('#chatHistory').scrollTop = $('#chatHistory').scrollHeight;"),
                wf:flush(room); %% we flush to websocket process by key
            Unknown -> error_logger:info_msg("Unknown Looper Message ~p",[Unknown])
        end,
        chat_loop().
    
And try to perform it in your favourite language/framework.

Clean codebase and pure render
------------------------------

We feel free to brake original Nitrogen compatibility because we want to have
clean codebase. However we still able to easily port old Nitrogen web sites to N2O.
E.g. we return back id and class semantics of HTML and removed html_id.
We simplify render without html_encode which should be handled by application layer.
nitrogen.js originaly created by Rusty Klophaus for XHR was removed due to pure
WebSocket nature of N2O and native jQuery handling.

    <input id="sendButton" type="button" class="sendButton button" value="Send"/>
    <script>$('#sendButton').bind('click',function anonymous(event) { 
          ws.send(Bert.encodebuf({source: Bert.binary('sendButton'), 
                                  pickle: Bert.binary('R2LH0INQAAAAWXicy2DKYEt...'),
                                  xforms: Bert.binary('undefined')}));});</script>

Prerequisites
-------------

To run N2O sites you need Erlang R14 or higher and basho rebar installed.
N2O works on Windows, Mac and Linux.

Kickstart Bootstrap
-------------------

To try N2O you need just fetch from Github and build. We don’t use fancy
scripts so building process is OTP compatible: bootstrap site is bundled
as Erlang release.


    $ git clone https://github.com/5HT/n2o-release
    $ cd n2o-release
    $ rebar get-deps
    $ rebar compile
    $ cd rels/web && rebar -f generate
    $ node/bin/node console

Now you can try: http://localhost:8000.

Start yourself depending N2O core
---------------------------------

If you want start with depending raw N2O core you shou should define
N2O http and websocket cowboy handlers and cowboy static handler in
Cowboy dispatch parameter:

    cowboy:start_http(http, 100, [{port, 8000}],
                              [{env, [{dispatch, dispatch_rules()}]}]),


    dispatch_rules() ->
        cowboy_router:compile(
           [{'_', [
                {["/static/[...]"], cowboy_static, [{directory, {priv_dir, ?APP, [<<"static">>]}},
                        {mimetypes, {fun mimetypes:path_to_mimes/2, default}}]}, 
                {["/websocket/[...]"], n2o_websocket, []},
                {'_', n2o_cowboy, []}
        ]}]).

And put minimal index.erl page:

    -module(index).
    -compile(export_all).
    -include_lib("n2o/include/wf.hrl").
    
    main() -> #span{text=<<"Hello">>}.

Developer scripts for Sync
--------------------------

For developing we use some script which is needed for linking source
directories with release lib directories and also link BERT, N2O
and jQuery javascript. After making release you should perform:

    $ ./nitrogen_static.sh
    $ ./release_sync.sh

Now you can edit site sources and sync will automaticaly
recompile and reload modules in release.

Credits
-------

* Maxim Sokhatsky
* Andrii Zadorozhnii
* Roman Shestakov
