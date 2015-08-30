N2O: Erlang Application Server
==============================

[![Build Status](https://travis-ci.org/synrc/n2o.svg?branch=master)](https://travis-ci.org/synrc/n2o)

Features
--------

* Formatters: **BERT**, JSON (changeable on the fly)
* Protocols: [N2O](http://5ht.co/n2o.htm)
* Endpoints: **WebSocket**, HTTP, [REST](http://synrc.github.io/rest)
* High Performance Protocol Relay
* Smallest possible codebase (1K LOC)
* BEAM/LING support on posix, arm, mips and xen platforms
* Single-file atomic packaging with [MAD](http://synrc.github.io/mad)
* Handlers
  * PubSub: MQS, GPROC
  * Templates: DTL, [NITRO](http://synrc.github.io/nitro)
  * Sessions: server driven
  * DOM Language: SHEN JavaScript Compiler
  * Error Logging: IO, LOGGER, [crashdump.io](http://crashdump.io)
  * Security: PLAIN, AES CBC 128
* Speed: **15K** **conn/s** easily
* Samples: Skyline (DSL), Games (SPA), Review (KVS), Sample (MAD)

Project Structure
-----------------

We polished directory tree to show you the several years of purity evolution.

```
├── endpoints
│   ├── cowboy
│   │   ├── bullet_handler.erl
│   │   ├── n2o_bullet.erl
│   │   ├── n2o_cowboy.erl
│   │   └── n2o_dynalo.erl
│   ├── n2o_document.erl
│   └── n2o_websocket.erl
├── formatters
│   ├── wf_convert.erl
│   └── wf_utils.erl
├── handlers
│   ├── n2o_auth.erl
│   ├── n2o_dynroute.erl
│   ├── n2o_error.erl
│   ├── n2o_io.erl
│   ├── n2o_log.erl
│   ├── n2o_mq.erl
│   ├── n2o_pickle.erl
│   ├── n2o_query.erl
│   ├── n2o_secret.erl
│   └── n2o_session.erl
├── n2o.app.src
├── n2o_app.erl
├── n2o_sup.erl
├── protocols
│   ├── n2o_binary.erl
│   ├── n2o_client.erl
│   ├── n2o_heart.erl
│   ├── n2o_nitrogen.erl
│   ├── n2o_rails.erl
│   └── n2o_text.erl
├── static_file.erl
├── wf.erl
└── wf_context.erl
```

That is all files related to what we called N2O application server,
the implementation of N2O core protocol. That is how it looks like:



Optional Dependencies
---------------------

For raw N2O use with BERT message formatter you need only one N2O dependecy,
but if you want to use DTL templates, JSON message formatter, SHEN JavaScript Compiler
or NITRO Nitrogen DSL you can plug all of them separately.

```erlang
{n2o,    ".*", {git, "git://github.com/synrc/n2o",          {tag, "master"} }},
{jsone, ".*",  {git, "git://github.com/sile/jsone.git",     {tag,"v0.3.3"}}},
{nitro,  ".*", {git, "git://github.com/synrc/nitro",        {tag, "master"} }},
{erlydtl,".*", {git, "git://github.com/evanmiller/erlydtl", {tag, "0.8.0"}  }},
```

Message Formatters
------------------

You can use any message formmatter at the bottom of N2O protocol.
The IO message of N2O protocol could be seen as follows:

```
1. BERT : {io,"console.log('hello')",1}
2. WAMP : [io,"console.log('hello')",1]
3. JSON : {name:io,eval:"console.log('hello')",data:1}
4. TEXT : IO console.log('hello') 1\n
5. XML  : <io><eval>console.log('hello')</eval><data>1</data></io>
```

Moreover you can switch channel termination formatter on the fly
in the same WebSocket session.

All Features in One snippet
---------------------------

```erlang
-module(index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

peer()    -> io_lib:format("~p",[wf:peer(?REQ)]).
message() -> wf:js_escape(wf:html_encode(wf:q(message))).
main()    -> #dtl{file="index",app=n2o_sample,bindings=[{body,body()}]}.
body() ->
    {ok,Pid} = wf:comet(fun() -> chat_loop() end),
    [ #panel{id=history}, #textbox{id=message},
      #button{id=send,body="Chat",postback={chat,Pid},source=[message]} ].

event(init) -> wf:reg(room);
event({chat,Pid}) -> Pid ! {peer(), message()};
event(Event) -> skip.

chat_loop() ->
    receive {Peer, Message} ->
       wf:insert_bottom(history,#panel{body=[Peer,": ",Message,#br{}]}),
       wf:flush(room) end, chat_loop().
```

Performance
-----------

We are using for measurement ab, httperf, wrk and siege, all of them. The most valuable storm
created by wrk and it is not achieved in real apps but could show us the internal throughput
of individual components. The most near to real life apps is siege who also make DNS lookup
for each request. So this data shows internal data throughput by wrk:

| Framework | Enabled Components | Speed | Timeouts |
|-----------|--------------------|-------|----------|
| PHP5 FCGI | Simple script with two <?php print "OK"; ?> | 5K | timeouts |
| ChicagoBoss| No sessions, No DSL, Simple DTL | 500 | no |
| Nitrogen  | No sessions, No DSL, Simple DTL | 1K | no |
| N2O       | All enabled, sessions, Template, heavy DSL | 7K | no |
| N2O       | Sessions enabled, template with two variables, no DSL | 10K | no |
| N2O       | No sessions, No DSL, only template with two vars | 15K | no |

Kickstart Bootstrap
-------------------

To try N2O you just need to clone a N2O repo from Github and build.
We use very small and powerful mad tool designed for our Web Stack.

    $ git clone git://github.com/5HT/n2o
    $ cd n2o/samples
    $ ./mad deps compile plan repl

Now you can try: [http://localhost:8000](http://localhost:8000)

LINUX NOTE: if you want to have online recompilation you should do at first:

    $ sudo apt-get install inotify-tools

Erlang version
--------------

We don't accept any reports of problems with ESL or Ubuntu packaging.
We do support only Erlang built from sources, official Windows package,
built with kerl or installed on Mac with brew. If you have problems
with your favourite Erlang package for your OS, please report issues
to package maintainer.

Posting Issues on Github
-------

Thank you for using N2O (you've made a wise choice) and your contributions
to help make it better. Before posting an issue in Github, please contact
us via the options listed below in the support section. Doing so will
help us determine whether your issue is a suggested feature, refactor
of existing code, bug, etc, that needs to be posted to GitHub for the
contributing development community of N2O to incorporate. DO NOT post
issues to GitHub related to misuses of N2O, all such issues will be closed.

Support
-------
* [![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/5HT/n2o?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
* IRC Channel #n2o on FreeNode 24/7

Documentation
-------

If you are new or need to determine whether the N2O architecture
and philosophy is a fit for your project

* Official N2O Book [PDF](https://synrc.com/apps/n2o/doc/book.pdf)

Windows Users
-------------

For windows you should install http://msys2.github.io and
appropriative packages to use Synrc Stack:

* pacman -S git

Credits
-------

* Maxim Sokhatsky -- core, shen, windows
* Dmitry Bushmelev -- endpoints, yaws, cowboy
* Andrii Zadorozhnii -- elements, actions, handlers
* Vladimir Kirillov -- mac, bsd, xen, linux support
* Andrey Martemyanov -- binary protocols
* Oleksandr Nikitin -- security
* Anton Logvinenko -- doc
* Roman Shestakov -- advanced elements, ct
* Jesse Gumm -- nitrogen, help
* Rusty Klophaus -- original author

OM A HUM
