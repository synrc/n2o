N2O: Distributed Application Server
===================================

[![Build Status](https://travis-ci.org/synrc/n2o.svg?branch=master)](https://travis-ci.org/synrc/n2o)

N2O is a embeddable message protocol loop library for
WebSocket, HTTP, MQTT and TCP servers. It provides basic
features, such as: process management; virtual nodes ring for
request processing; session, encoding, mq, cache and log services.
It also includes poor man's bridges to server's endpoints.

Features
--------

* Purpose: High performance protocol relay
* Endpoints: WebSockets, MQTT, TCP
* Codebase: 1K LOC (Erlang), 500 LOC (JavaScript)
* Buildtools: REBAR, MAD
* Templates: DTL, NITRO
* Hosts: COWBOY, EMQ, MOCHIWEB
* Samples: REVIEW (1), SAMPLE (2)

Idea
----

N2O was created to bring clarity and sanity to software development.
The distribution model is per file basis with ISC license.

Kernel
------

The core modules provide OTP start and N2O entry point.

* [n2o](https://ws.n2o.space/man/n2o.htm) — N2O OTP Supervisor and Application
* [n2o_async](https://ws.n2o.space/man/n2o_async.htm) — N2O Async Processes
* [n2o_proto](https://ws.n2o.space/man/n2o_proto.htm) — N2O Loop

MQTT
----

MQTT version is implemented as RPC over MQ pattern.
N2O service worker started as ring of virtual nodes each runs N2O loop.

* [n2o_vnode](https://ws.n2o.space/man/n2o_vnode.htm) — N2O Virtual Node
* [n2o_auth](https://ws.n2o.space/man/n2o_auth.htm) — N2O Auth
* [n2o_ring](https://ws.n2o.space/man/n2o_ring.htm) — N2O Ring

```
$ mad app mqtt review
$ cd review
$ mad dep com pla rep
$ open http://127.0.0.1:8000
```

WebSocket
---------

N2O Loop is directly connected and runned inside context of WebSocket handler.
Usually in Erlang we use `syn` or `gproc` OTP message buses.
As such buses are optional in MQTT setup we include bus drivers in WebSocket package.

* [n2o_stream](https://ws.n2o.space/man/n2o_stream.htm) — COWBOY and XHR bridge
* [n2o_heart](https://ws.n2o.space/man/n2o_heart.htm) — PING protocol
* [n2o_cowboy](https://ws.n2o.space/man/n2o_cowboy.htm) — COWBOY API
* [n2o_cowboy2](https://ws.n2o.space/man/n2o_cowboy2.htm) — COWBOY 2 API
* [n2o_gproc](https://ws.n2o.space/man/n2o_gproc.htm) — GPROC bus backend
* [n2o_syn](https://ws.n2o.space/man/n2o_syn.htm) — SYN bus backend

```
$ mad app web sample
$ cd sample
$ mad dep com pla rep
$ open http://127.0.0.1:8001
```

Protocols
---------

N2O is shipped with 3 protocols, which could be omited or extended.

* [n2o_nitro](https://ws.n2o.space/man/n2o_nitro.htm) — N2O Nitrogen web framework protocol
* [n2o_ftp](https://ws.n2o.space/man/n2o_ftp.htm) — N2O File protocol
* [n2p_heart](https://ws.n2o.space/man/n2o_heart.htm) — N2O Heart protocol

Services
--------

Formatters, Loggers, Sessions, etc. Optional.

* [n2o_bert](https://ws.n2o.space/man/n2o_bert.htm) — BERT encoder/decoder
* [n2o_json](https://ws.n2o.space/man/n2o_json.htm) — JSON encoder/decoder
* [n2o_secret](https://ws.n2o.space/man/n2o_secret.htm)  — AES/CBC-128 encoder/decoder
* [n2o_session](https://ws.n2o.space/man/n2o_session.htm) — ETS session backend
* [n2o_io](https://ws.n2o.space/man/n2o_io.htm) — IO loger backend

JavaScript
----------

* [bert.js](https://ws.n2o.space/man/bert.js.htm) — BERT encoder/decoder
* [utf8.js](https://ws.n2o.space/man/utf8.js.htm) — UTF8 encoder/decoder
* [bullet.js](https://ws.n2o.space/man/bullet.js.htm)  — HEART protocol
* [nitrogen.js](https://ws.n2o.space/man/nitrogen.js.htm)  — NITRO protocol
* [ftp.js](https://ws.n2o.space/man/ftp.js.htm)  — FTP protocol
* [n2o.js](https://ws.n2o.space/man/n2o.js.htm) — N2O protocol looper

Literature
----------

* N2O Book [PDF](http://synrc.com/apps/n2o/doc/book.pdf)

