N2O: Erlang Application Server
==============================

[![Build Status](https://travis-ci.org/synrc/n2o.svg?branch=master)](https://travis-ci.org/synrc/n2o)

Features
--------

* Formatters: **BERT**, JSON — changeable on the fly
* Protocols: [N2O](http://5ht.co/n2o.htm), [NITRO](http://5ht.co/n2o.htm), [SPA](http://5ht.co/n2o.htm), [FTP](http://5ht.co/ftp.htm)
* Endpoints: **WebSocket**, HTTP, [REST](http://synrc.github.io/rest)
* High Performance Protocol Relay
* Smallest possible codebase — **1K** **LOC**
* BEAM/LING support on posix, arm, mips and xen platforms
* Single-file atomic packaging with [MAD](http://synrc.github.io/mad)
* Handlers
  * PubSub: MQS, GPROC, SYN
  * Templates: DTL, [NITRO](http://synrc.github.io/nitro)
  * Sessions: server driven
  * DOM Language: SHEN JavaScript Compiler
  * Error Logging: IO, LOGGER
  * Security: PLAIN, AES CBC 128
* Speed: **30K** **conn/s** at notebook easily
* Samples: Skyline (DSL), Games (SPA), Review (KVS), Sample (MAD)

Optional Dependencies
---------------------

N2O comes with BERT message formatter support out of the box, and you only need
one N2O dependency in this case. Should you need DTL templates, JSON message formatter, 
SHEN JavaScript Compiler or NITRO Nitrogen DSL you can plug all of them in separately:

```erlang
{n2o,    ".*",{git,"git://github.com/synrc/n2o",         {tag, "2.8"}}},
{nitro,  ".*",{git,"git://github.com/synrc/nitro",       {tag, "2.8"}}},
{shen,   ".*",{git,"git://github.com/synrc/shen",        {tag, "1.5"}}},
{jsone,  ".*",{git,"git://github.com/sile/jsone.git",    {tag,"v0.3.3"}}},
{erlydtl,".*",{git,"git://github.com/evanmiller/erlydtl",{tag,"0.8.0"}}},
```

Message Formatters
------------------

You can use any message formmatter at the bottom of N2O protocol.
IO messages supported by the N2O protocol are as follows:

```
1. BERT : {io,"console.log('hello')",1}
2. WAMP : [io,"console.log('hello')",1]
3. JSON : {name:io,eval:"console.log('hello')",data:1}
4. TEXT : IO console.log('hello') 1\n
5. XML  : <io><eval>console.log('hello')</eval><data>1</data></io>
```

Besides, you can even switch a channel termination formatter on the fly
within one WebSocket session.

All Features in One snippet
---------------------------

```erlang
-module(index).
-compile(export_all).
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/wf.hrl").

peer()    -> io_lib:format("~p",[wf:peer(?REQ)]).
message() -> wf:js_escape(wf:html_encode(wf:q(message))).
main()    -> #dtl{file="index",app=n2o_sample,bindings=[{body,body()}]}.
body() ->
    {Pid,_} = wf:async(fun(X) -> chat_loop(X) end),
    [ #panel{id=history}, #textbox{id=message},
      #button{id=send,body="Chat",postback={chat,Pid},source=[message]} ].

event(init) -> wf:reg(room);
event({chat,Pid}) -> Pid ! {peer(), message()};
event(Event) -> skip.

chat_loop({Peer, Message} ) ->
       wf:insert_bottom(history,#panel{body=[Peer,": ",Message,#br{}]}),
       wf:flush(room) end.
```

Performance
-----------

ab, httperf, wrk and siege are all used for measuring performance. 
The most valuable request hell is created by wrk and even though it 
is not achievable in real apps, it can demonstrate internal throughput 
of certain individual components. 

The nearest to real life apps is siege which also performs a DNS lookup
for each request. The data below shows internal data throughput by wrk:

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

To try N2O you  need to clone a N2O repo from Github and build it.
We use a very small and powerful tool called mad designed specially for our Web Stack.

    $ git clone git://github.com/synrc/n2o
    $ cd n2o/samples
    $ ./mad deps compile plan repl

Now you can try it out: [http://localhost:8000](http://localhost:8000)

LINUX NOTE: if you want to have online recompilation you should install `inotify-tools` first:

    $ sudo apt-get install inotify-tools

Tests
-----

    $ cd tests
    $ npm install -g casperjs
    $ casperjs test casper

Erlang version
--------------

We don't accept any reports of problems related to ESL or Ubuntu packaging.
We only support Erlang built from sources, official Windows package,
built with kerl or installed on Mac with homebrew. If you have any problems
with your favourite Erlang package for your OS, please report issues
to package maintainer.

Posting Issues on Github
-------

Thank you for using N2O (you've made a wise choice) and your contributions
to help make it better. Before posting an issue on Github, please contact
us via the options listed below in the support section. Doing so will
help us determine whether your issue is a suggested feature, refactor
of existing code, bug, etc, that needs to be posted to GitHub for the
contributing development community of N2O to incorporate. DO NOT post
issues to GitHub related to misuses of N2O, all such issues will be closed.

Support
-------
* [![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/synrc/n2o?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
* IRC Channel #n2o on FreeNode 24/7

Documentation
-------

If you are new or you need to decide whether the N2O architecture
and philosophy is a fit for your project

* Official N2O Book [PDF](http://synrc.com/apps/n2o/doc/book.pdf)

Windows Users
-------------

For windows you should install http://msys2.github.io and
appropriative packages to use Synrc Stack:

* pacman -S git

Credits
-------

* Maxim Sokhatsky — core, shen, windows
* Dmitry Bushmelev — endpoints, yaws, cowboy
* Andrii Zadorozhnii — elements, actions, handlers
* Vladimir Kirillov — mac, bsd, xen, linux support
* Andrey Martemyanov — binary protocols
* Oleksandr Nikitin — security
* Anton Logvinenko — doc
* Roman Shestakov — advanced elements, ct
* Jesse Gumm — nitrogen, help
* Rusty Klophaus — original author

OM A HUM
