N2O: Distributed Application Server
===================================

[![Build Status](https://travis-ci.org/synrc/n2o.svg?branch=master)](https://travis-ci.org/synrc/n2o)

N2O is a embeddable message protocol loop library for
WebSocket, HTTP, MQTT and TCP servers. It provides basic
features, such as process management, virtual nodes ring for
request processing, session, frame encoding, mq and caching services.

Core Features
-------------

* Purpose: High performance protocol relay
* Endpoints: WebSockets, MQTT, TCP
* Codebase: 1K LOC (Erlang), 500 LOC (JavaScript)
* Dialyzer: REBAR, REBAR3, MAD, MIX
* Hosts: COWBOY, EMQ, MOCHIWEB, RING, TCP, UDP

Protocol Extensions
-------------------

* Templates: DTL, <a href="https://nitro.n2o.space">NITRO</a>
* Databases [KVS, <a href="https://kvx.n2o.space">KVX</a>]: FS, MNESIA, ROCKSDB, RIAK, REDIS
* Business Processes: <a href="https://bpe.n2o.space">BPE</a> (BPMN 2.0), SCM, ERP, CRM
* Instant Messaging: <a href="https://chat.n2o.space">CHAT</a>
* HTTP API: <a href="https://rest.n2o.space">REST</a> (proplist/JSON)
* <a href="https://active.n2o.space">ACTIVE</a> Reloading: Linux, Windows, Mac

Samples
-------
* MQTT Chat: <a href="https://review.n2o.space">REVIEW TT</a> (8000)
* WebSocket Chat: <a href="https://sample.n2o.space">SAMPLE WS</a> (8001)
* Business Forms: <a href="http://forms.n2o.space">FORMS</a> (8002, HTTP)
* Online Client Bank: <a href="https://bank.n2o.space">BANK</a> (8003)
* Instant Messaging: <a href="https://chat.n2o.space">CHAT</a> (8042)

Motivation
----------

N2O was created to bring clarity and sanity to software development.
The distribution model is per file basis with ISC license.

Kernel
------

The core modules provide OTP start and N2O entry point.

* [n2o](https://ws.n2o.space/man/n2o.htm) — N2O OTP Supervisor and Application
* [n2o_pi](https://ws.n2o.space/man/n2o_pi.htm) — N2O Processes
* [n2o_proto](https://ws.n2o.space/man/n2o_proto.htm) — N2O Loop
* [n2o_ring](https://ws.n2o.space/man/n2o_ring.htm) — N2O Ring

MQTT
----

MQTT version is implemented as RPC over MQ pattern.
N2O service worker started as ring of virtual nodes each runs N2O loop.

* [n2o_mqtt](https://ws.n2o.space/man/n2o_vnode.htm) — N2O MQTT Virtual Node
* [n2o_auth](https://ws.n2o.space/man/n2o_auth.htm) — N2O Auth

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
* [n2o_ws](https://ws.n2o.space/man/n2o_wsnode.htm) — N2O WebSocket Virtual Node
* [n2o_heart](https://ws.n2o.space/man/n2o_heart.htm) — PING protocol
* [n2o_cowboy](https://ws.n2o.space/man/n2o_cowboy.htm) — COWBOY API
* [n2o_gproc](https://ws.n2o.space/man/n2o_gproc.htm) — GPROC bus backend
* [n2o_syn](https://ws.n2o.space/man/n2o_syn.htm) — SYN bus backend

```
$ mad app web sample
$ cd sample
$ mad dep com pla rep
$ open http://127.0.0.1:8001/app/index.htm
```

Protocols
---------

N2O ships with 3 optional protocols.

* [n2o_nitro](https://ws.n2o.space/man/n2o_nitro.htm) — N2O Nitrogen web framework protocol
* [n2o_ftp](https://ws.n2o.space/man/n2o_ftp.htm) — N2O File protocol
* [n2o_heart](https://ws.n2o.space/man/n2o_heart.htm) — N2O Heart protocol

Services
--------

Formatters, Sessions, etc. Optional.

* [n2o_bert](https://ws.n2o.space/man/n2o_bert.htm) — BERT encoder/decoder
* [n2o_json](https://ws.n2o.space/man/n2o_json.htm) — JSON encoder/decoder
* [n2o_secret](https://ws.n2o.space/man/n2o_secret.htm)  — AES/CBC-128 encoder/decoder
* [n2o_session](https://ws.n2o.space/man/n2o_session.htm) — ETS session storage

JavaScript
----------

* [bert.js](https://ws.n2o.space/man/bert.js.htm) — BERT encoder/decoder
* [utf8.js](https://ws.n2o.space/man/utf8.js.htm) — UTF8 encoder/decoder
* [ieee754.js](https://ws.n2o.space/man/ieee754.js.htm) — IEEE754 encoder/decoder
* [heart.js](https://ws.n2o.space/man/heart.js.htm) — HEART protocol
* [nitro.js](https://ws.n2o.space/man/nitro.js.htm) — NITRO protocol
* [ftp.js](https://ws.n2o.space/man/ftp.js.htm)  — FTP protocol
* [n2o.js](https://ws.n2o.space/man/n2o.js.htm) — N2O protocol loop
* [mq.js](https://ws.n2o.space/man/mq.js.htm) — MQTT client

Literature
----------

* N2O Book [PDF](http://synrc.com/apps/n2o/doc/book.pdf)

