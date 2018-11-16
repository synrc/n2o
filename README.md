N2O: Distributed Application Server
===================================

[![Build Status](https://travis-ci.org/synrc/mqtt.svg?branch=master)](https://travis-ci.org/synrc/mqtt)

N2O is a embeddable message protocol loop library for
WebSocket, HTTP, MQTT and TCP servers. It provides basic
features, such as: process management; virtual nodes ring for
request processing; session, encoding, mq, cache and log services.
It also includes poor man's bridges to server's endpoints.

Features
--------

* Purpose: High performance protocol relay
* Endpoints: WebSockets, MQTT, TCP
* Codebase: 1K LOC
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

* [n2o](https://mqtt.n2o.space/man/n2o.htm) — N2O OTP Supervisor and Application
* [n2o_async](https://mqtt.n2o.space/man/n2o_async.htm) — N2O Async Processes
* [n2o_proto](https://mqtt.n2o.space/man/n2o_proto.htm) — N2O Loop

MQTT
----

MQTT version is implemented as RPC over MQ pattern.
N2O service worker started as ring of virtual nodes each runs N2O loop.

* [n2o_vnode](https://mqtt.n2o.space/man/n2o_vnode.htm) — N2O Virtual Node
* [n2o_auth](https://mqtt.n2o.space/man/n2o_auth.htm) — N2O Auth
* [n2o_ring](https://mqtt.n2o.space/man/n2o_ring.htm) — N2O Ring

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

* [n2o_stream](https://mqtt.n2o.space/man/n2o_stream.htm) — COWBOY and XHR bridge
* [n2o_heart](https://mqtt.n2o.space/man/n2o_heart.htm) — PING protocol
* [n2o_cowboy](https://mqtt.n2o.space/man/n2o_cowboy.htm) — COWBOY API
* [n2o_cowboy2](https://mqtt.n2o.space/man/n2o_cowboy2.htm) — COWBOY 2 API
* [n2o_gproc](https://mqtt.n2o.space/man/n2o_gproc.htm) — GPROC bus backend
* [n2o_syn](https://mqtt.n2o.space/man/n2o_syn.htm) — SYN bus backend

```
$ mad app web sample
$ cd sample
$ mad dep com pla rep
$ open http://127.0.0.1:8001
```

Protocols
---------

N2O is shipped with 3 protocols, which could be omited or extended.

* [n2o_nitro](https://mqtt.n2o.space/man/n2o_nitro.htm) — N2O Nitrogen web framework protocol
* [n2o_ftp](https://mqtt.n2o.space/man/n2o_ftp.htm) — N2O File protocol
* [n2p_heart](https://mqtt.n2o.space/man/n2o_heart.htm) — N2O Heart protocol

Services
--------

Formatters, Loggers, Sessions, etc. Optional.

* [n2o_bert](https://mqtt.n2o.space/man/n2o_bert.htm) — BERT encoder/decoder
* [n2o_json](https://mqtt.n2o.space/man/n2o_json.htm) — JSON encoder/decoder
* [n2o_secret](https://mqtt.n2o.space/man/n2o_secret.htm)  — AES/CBC-128 encoder/decoder
* [n2o_session](https://mqtt.n2o.space/man/n2o_session.htm) — ETS session backend
* [n2o_io](https://mqtt.n2o.space/man/n2o_io.htm) — IO loger backend

Literature
----------

* N2O Book [PDF](http://synrc.com/apps/n2o/doc/book.pdf)

