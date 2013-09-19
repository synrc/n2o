N2O: Erlang Web Framework
=========================

Nitrogen 2x Optimized
---------------------

Information for Nitrogen users:

* All actions are triggered through single endpoint
* Dropped support of simple_brigde
* Dependency on Bert, jQuery and Bullet only
* no JSON encoding for client/server data transfer, no use of urlencode and Nitrogen.js
* Enough compatibility with original Nitrogen to convert Nitrogen sites/elements to N2O
* Proper id and class attributes
* Several times faster that original Nitrogen
* GProc process registry instead of nprocreg

New features
------------

* Twitter Bootstrap compatible rendering
* Optimized for latency: deffered JavaScript rendering
* Supports optional Zepto library for non-IE browsers
* XHR fallback through Bullet for legacy browsers
* Clean codebase without additional layers
* One process per page during lifetime
* Works heavy coupled within Cowboy processes
* Page construction from Erlang binaries
* Custom template engines as elements: DTL, SGTE, ET
* Advanced element collection: Tabs, Grid, Viz.js, Mandala
* Rapid REST apps prototyping with REST handlers

WebSockets transport
--------------------

N2O was started as the first Erlang Web Framework that fully relies on 
WebSocket transport for client/server communication.
Compatibility with Nitrogen was mostly retained and many new improvements were made.
Such as binary page construction, binary data transfer, events trigger over WebSocket channel,
minumum process spawns, use of Cowboy processes to run N2O html rendering.
N2O page rendering is several times faster than with the original Nitrogen.

Binary events over WebSockets
-----------------------------

N2O does not use JSON to encode data transfer between client/server. 
Instead, all data communication is encoded with native 
Erlang External Term Format. For that Berg.js library is used.
Is it as simple as calling Bert.decode(msg) and this allows 
to avoid complexity on the server-side. 
Please refer to http://bert-rpc.org for more information.

Optimized for speed
-------------------

Original Nitrogen was tested in production under high-load and
we decided to drop nprocreg process registry along with
action_comet process creation. N2O creates only a single
process for async websocket handler, all async operations
are handled within Cowboy processes.

Why Erlang in Web?
------------------

We have measured the performance of most of the existing modern web frameworks
created with functional languages and Cowboy is still the king. You can see
raw HTTP performance of functional and C-like languages with
concurrent primitives (Go, D and Rust) on VAIO Z notebook
with i7640M processor:

![WebServers](http://synrc.com/lj/webcompare/connections.png)

N2O greatly outperforms Nitrogen stack and is only ~2X slower than
the raw HTTP Cowboy performance thus beating rendering performance
of any other functional web framework several times over.
It is certanly faster than raw HTTP node.js performance!

Reference to list of modern web frameworks: http://gist.github.com/5HT/5522302

Desktop and Mobile Applications
-------------------------------

There are two approaches to design communications between client/server.
The first is called data-on-wire, where only data transfers on channel
through RPC, REST in form of JSON, XML or Binary. All rendering in first
type are being made on client side. This calls rich client and mostly
fits for desktop. The examples are Chaplin/CoffeScript and Meteor/JavaScript.

The other approach is to send server prerendered part of pages and javascript,
and on client side only replace HTML parts and execute JavaScript.
This approach better fits for mobile applications, where client doesn't
have much resources.

Using N2O you could create both types of applications: using N2O REST framework
for first type of application based on Cowboy REST features along with
DTL templates for initial HTML renderings, and also Nitrogen DSL-based approach to model
parts of the pages as widgets and control elements thanks
to rich Nitrogen elements collections provided by Nitrogen community.

So, in cases when your system is built around Erlang infrastructure,
N2O is the best choice that you could made for fast prototyping,
simpicity of use, codebase maintanance, etc. Despite HTML tranfer over the wire,
you will still have access to all your erlang services directly.

Templates vs DSL
----------------

We liked Nitrogen for simple and elegant way of typed HTML
page construction with DSL base on host language similar to
Scala Lift, OCaml Ocsigen and Haskell Blaze. It helps to develop
reusable control elements and components in host language.

Template-based approach pushes programmers to
deal with raw HTML, like Yesod, ASP, PHP, JSP, Rails, Yaws,
ChicagoBoss. It help to define the page in terms of top-level 
consist of controls, playholders and panels. So N2O combine both approaches.

Main N2O attraction is the fast prototyping. We also use it
in large scale projects. Here is the complete Web Chat example
working with WebSockets that demonstrate the use of Templates, DSL
and async interprocesses communications:

    -module(chat).
    -compile(export_all).
    -include_lib("n2o/include/wf.hrl").

    main() ->
        Title = wf_render_elements:render_elements(title()),
        Body = wf_render_elements:render_elements(body()),
        [ #dtl{file = "index", bindings=[{title,Title},{body,Body}]} ].

    title() -> <<"N2O">>.

    body() -> %% area of http handler
        {ok,Pid} = wf:comet(fun() -> chat_loop() end),
      [ #span { text= <<"Your chatroom name: ">> },
        #textbox { id=userName, text= <<"Anonymous">> },
        #panel { id=chatHistory, class=chat_history },
        #textbox { id=message },
        #button { id=sendButton, text= <<"Send">>,
                  postback={chat,Pid}, source=[userName,message] },
        #panel { id=status } ].

    event({chat,Pid}) -> %% area of websocket handler
        Username = wf:q(userName),
        Message = wf:q(message),
        Terms = [ #span { text="Message sent" }, #br{} ],
        wf:insert_bottom(chatHistory, Terms),
        wf:reg(room),
        Pid ! {message, Username, Message};

    event(Event) -> error_logger:info_msg("Unknown Event: ~p", [Event]).

    chat_loop() -> %% background worker ala comet
        receive
            {message, Username, Message} ->
                Terms = [ #span { text=Username }, ": ",
                          #span { text=Message }, #br{} ],
                wf:insert_bottom(chatHistory, Terms),
                wf:flush(room); %% we flush to websocket process by key
            Unknown -> error_logger:info_msg("Unknown Looper Message ~p",[Unknown])
        end,
        chat_loop().

And try to compare how this functionality would be implemented
with your favourite language/framework.

Clean codebase
------------------------------

We feel free to break some of the compatability with the original Nitrogen project,
mostly because we want to have a clean codebase and fastest speed.
However, it is still possible to easily port Nitrogen web sites to N2O.
E.g. N2O returns id and class semantics of HTML and not html_id.
We simplified rendering by not using html_encode which should be handled by the application layer.
Nitrogen.js that was originally created by Rusty Klophaus, has been removed due to pure simplified
WebSocket nature of N2O. We added XHR fallback handling through Extend Bullet by Loïc Hoguin.
We dropped simple_bridge and optimize N2O on every level for you to be sure its fastest way
to develop application on erlang.

    <input id="sendButton" type="button" class="sendButton button" value="Send"/>
    <script>$('#sendButton').bind('click',function anonymous(event) {
          ws.send(Bert.encodebuf({source: Bert.binary('sendButton'),
                                  pickle: Bert.binary('R2LH0INQAAAAWXicy2DKYEt...'),
                                  xforms: Bert.binary('undefined')}));});</script>

Reduced Latency
---------------

The secret of reduced latency is simple. We try to deliver rendered HTML as soon as
possible and render JavaScript only after WebSocket initialization. We use thre steps
and three erlang processes for achieve that.

![N2O Page Lifetime](http://synrc.com/lj/page-lifetime.png)

In first HTTP handler we render only HTML and all created by the way action is
stored in created transition process. 

    transition(Actions) -> receive {'N2O',Pid} -> Pid ! Actions end.

HTTP handler dies immediately after terurning HTML. Transition process waits for
retrival request from future WebSocket handler.

Just after receiving HTML browser initiates WebSocket connection and WebSocket
handler arise. After returning actions transition process dies and from now on
WebSocket handler stay alone. Thus initial phase done.

After that through WebSocket channel all event comes from browser to server and
handler by N2O, who renders elements to HTML and actions to JavaScript.

Performance
-----------

We are using for measurement ab, httperf, wrk and siege, all of them. The most valuable storm
created by wrk and it is not achieved in real apps but could show us the internal throughput
of individual components. The most near to real life apps is siege who also make DNS lookup
for each request. So this data shows internal data throughput by wrk:

| Framework | Enabled Components | Speed | Timeouts |
|-----------|--------------------|-------|----------|
| PHP5 FCGI | Simple script with two <?php print "OK"; ?> terms inside | 5K | timeouts |
| Nitrogen  | No sessions, No DSL, Simple template with two variable | 1K | no |
| N2O       | All enabled, sessions, Template, heavy DSL | 7K | no |
| N2O       | Sessions enabled, template with two variables, no DSL | 10K | no |
| N2O       | No sessions, No DSL, only template with two vars | 15K | no |

Prerequisites
-------------

To run N2O sites you need Erlang R15 or higher and basho rebar installed.
N2O works on Windows, Mac and Linux. 

NOTE: the work of sync application on Windows is limited.

    $ sudo apt-get install build-essential libncurses5-dev openssl libssl-dev m4
    $ curl -O https://github.com/spawngrid/kerl/blob/master/kerl
    $ chmod a+x kerl
    $ echo KERL_CONFIGURE_OPTIONS="--enable-threads --enable-smp-support \
           --enable-m64-build --without-javac --enable-kernel-poll" > ~/.kerlrc
    $ kerl update releases
    $ kerl build R16B01 r16b01
    $ kerl install r16b01 /usr/lib/erlang
    $ . /usr/lib/erlang/activate

Kickstart Bootstrap
-------------------

To try N2O you just need to clone a N2O repo from Github and build. We don’t use fancy
scripts so building process is OTP compatible: bootstrap site is bundled as Erlang release.

    $ git clone https://github.com/5HT/n2o
    $ cd n2o/samples
    $ rebar get-deps
    $ rebar compile
    $ ./nitrogen_static.sh
    $ ./release.sh
    $ ./release_sync.sh
    $ ./start.sh

Now you can try: [http://localhost:8000](http://localhost:8000)

Start yourself depending N2O core
---------------------------------

If you want dependency on the raw N2O core you should define
N2O http and websocket (ws endpoint) cowboy handlers and cowboy static handler as
Cowboy dispatch parameter:

    cowboy:start_http(http, 100, [{port, 8000}],
                                 [{env, [{dispatch, dispatch_rules()}]}]),


    dispatch_rules() ->
        cowboy_router:compile(
           [{'_', [
                {["/static/[...]"], cowboy_static,
                    [{directory, {priv_dir, ?APP, [<<"static">>]}},
                     {mimetypes, {fun mimetypes:path_to_mimes/2, default}}]},
                {"/ws/[...]", bullet_handler, [{handler, n2o_bullet}]},
                {'_', n2o_cowboy, []}
        ]}]).

And put minimal index.erl page:

    -module(index).
    -compile(export_all).
    -include_lib("n2o/include/wf.hrl").

    main() -> [ #span{text = <<"Hello">>} ].

Developer scripts for Sync
--------------------------

For developing we use some scripts which are needed for linking source
directories with release lib directories and also links to BERT, N2O
and jQuery javascript. After making release you should run:

    $ ./nitrogen_static.sh
    $ ./release_sync.sh

Now you can edit site sources and sync will automaticaly recompile
and reload modules in release.

Credits
-------

* Maxim Sokhatsky
* Andrii Zadorozhnii
* Roman Shestakov
* Anton Logvinenko
* Jesse Gumm
* Rusty Klophaus

OM A HUM
