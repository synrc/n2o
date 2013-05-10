N2O: Erlang Web Framework on WebSockets
=======================================

Nitrogen 2 Optimized
--------------------

Information for Nitrogen users:

* Page construction from binaries
* Do all Actions are triggered through WebSocket channel
* Work within Cowboy processes
* Bert/jQuery
* no JSON, urlencode and nitrogen.js
* Enough Compatible with original Nitrogen to convert Nitrogen sites to N2O
* Clean codebase
* Separate Advanced Nitrogen elements
* GProc process registry instead of nprocreg
* Proper id and class attributes

WebSockets transport
--------------------

N2O was started as the first Erlang Web Framework that fully relies on WebSocket transport for client/server communication.
Compatibility with Nitrogen was mostly retained and many new improvements were made.
E.g. binary page construction, binary data transfer, events trigger over WebSocket channel,
minumum process spawns, use of Cowboy processes to run N2O html rendering.
N2O page rendering is several times faster than in original Nitrogen.


Binary events over WebSockets
-----------------------------

N2O does not use JSON to encode data transfer between client/server. Instead all data communications
are encoded with native Erlang External Term Format. For that Berg.js library is used.
Is it as simple as calling Bert.decode( msg ) and this allows to avoid complexity on the server-side.
Please refer to http://bert-rpc.org for more information.

Optimized for speed
-------------------

Original Nitrogen was tested in production under high-load and
we decided to drop nprocreg process registry along with
action_comet process creation. N2O creates only a single process for async websocket handler,
all async operations are handled within Cowboy processes.

Why Erlang in Web?
------------------

We have measured the performance of most of the existing modern web frameworks
created with functional languages and Cowboy is still the king. You can see
raw HTTP performance of functional and C-like languages with
concurrent primitives (Go, D and Rust) on VAIO Z notebook
with i7640M processor:

![WebServers](http://synrc.com/lj/webcompare/connections.png)

N2O greatly outperforms Nitrogen stack and is only ~2X slower than
the raw HTTP Cowboy performance thus rising rendering performance
several times in comparison to any other functional web framework.
It is certanly faster than raw HTTP node.js performance!

Sending HTML5 over wire?
------------------------

N2O does that. However, we agree that in some cases it is better to send
only problem domain specific data over network (in JSON or Binary format) as you would do with
Chaplin / CoffeScript and Meteor / JavaScript.
However, Meteor is Node-based. And with “everything on the client” model
it would not be possible to prototype services easily with your Erlang infrastructure.
You should always prototype a server side protocol along with the client.

So, in cases when your system is built around Erlang infrastructure,
N2O is the best choice that you could find for fast prototyping,
simpicity of use, codebase maintanance, etc. Despite HTML5 tranfered over the wire,
you will still have access to all your erlang services directly.

Templates vs DSL
----------------

We liked Nitrogen for simple and elegant way of typed HTML
page construction similar to Scala Lift, OCaml Ocsigen and
Haskell Happstack. Template-based approach pushes programmers to
deal with raw HTML, like Yesod, ASP, PHP, JSP, Rails, Yaws,
ChicagoBoss. N2O goes further than that and optimizes HTML rendering from
binary iolists instead of slower Erlang lists as in Nitrogen.

Main N2O attraction is fast prototyping. We also use it
in large scale projects. Here is complete Web Chat example
working with WebSockets:

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

And try to compare how this functionality would be implemented in your favourite language / framework.

Clean codebase
------------------------------

We feel free to brake some of the compatability with the original Nitrogen project, mostly because we want to have
a clean codebase. However, it is still possible to easily port Nitrogen web sites to N2O.
E.g. N2O returns id and class semantics of HTML and not html_id.
We simplify rendering by not using html_encode which should be handled by the application layer.
Nitrogen.js that was originally created by Rusty Klophaus for XHR has been removed due to pure
WebSocket nature of N2O and native jQuery handling. There are plans to add XHR support by
using some of the ideas from Bullet.js - such as fallback from websockets to XHR.

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

To try N2O you just need to clone a N2O repo from Github and build. We don’t use fancy
scripts so building process is OTP compatible: bootstrap site is bundled as Erlang release.

    $ git clone https://github.com/5HT/n2o-release
    $ cd n2o-release
    $ rebar get-deps
    $ rebar compile
    $ cd rels/web && rebar -f generate
    $ node/bin/node console

Now you can try: http://localhost:8000.

Start yourself depending N2O core
---------------------------------

If you want dependency on the raw N2O core you should define
N2O http and websocket cowboy handlers and cowboy static handler as
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

For developing we use some scripts which are needed for linking source
directories with release lib directories and also links to BERT, N2O
and jQuery javascript. After making release you should run:

    $ ./nitrogen_static.sh
    $ ./release_sync.sh

Now you can edit site sources and sync will automaticaly recompile and reload modules in release.

Credits
-------

* Maxim Sokhatsky
* Andrii Zadorozhnii
* Roman Shestakov
* Jesse Gumm
* Rusty Klophaus

OM A HUM
