# N2O: TCP MQTT WebSocket

[![Actions Status](https://github.com/synrc/n2o/workflows/mix/badge.svg)](https://github.com/synrc/n2o/actions)
[![Hex pm](http://img.shields.io/hexpm/v/n2o.svg?style=flat)](https://hex.pm/packages/n2o)

N2O is an embeddable message protocol loop library for WebSocket, HTTP, MQTT
and TCP servers. It provides basic features, such as process management,
virtual nodes ring for request processing, sessions, frame encoding, and
unified API for external mq and caching services.

## Core Features

* Purpose: High performance protocol relay
* Endpoints: WebSockets, MQTT, TCP
* Codebase: 700 LOC (Erlang), 500 LOC (JavaScript)
* Dialyzer: REBAR, REBAR3, MAD, MIX
* Hosts: BANDIT, COWBOY, EMQ, MOCHIWEB, RING, TCP, UDP
* PubSub: GPROC, SYN, PG2
* Formatters: JSON, BERT, ASN.1

## Protocol Extensions

* Templates: DTL, [NITRO](https://nitro.n2o.dev)
* Abstract Database Layer [KVS](https://kvs.n2o.dev"): FS, MNESIA, ROCKSDB, RIAK, REDIS
* Business Processes: [BPE](https://bpe.n2o.dev) (BPMN 2.0), SCM, ERP, CRM
* HTTP API: [REST](https://rest.n2o.dev) (proplist/JSON)
* [ACTIVE](https://active.n2o.dev) Reloading: GNU/Linux, Windows, macOS

## Basic Samples

* MQTT Chat: [REVIEW TT](https://review.n2o.dev) (8000)
* WebSocket Chat: [SAMPLE WS](https://sample.n2o.dev) (8001)

## Enterprise Samples

* Online Client Bank: [BANK](https://fin.erp.uno) (8041)
* Instant Messaging: [CHAT](https://chat.n2o.dev) (8042)
* Product Lifecycle Management: [PLM](https://plm.erp.uno) (8043)

## Motivation

N2O was created to bring clarity and sanity to software development.
The distribution model is per file basis with ISC license.

## Kernel

The core modules provide OTP start and N2O entry point.

* [n2o](https://ws.n2o.dev/man/n2o.htm) — N2O OTP Supervisor and Application
* [n2o_pi](https://ws.n2o.dev/man/n2o_pi.htm) — N2O Processes
* [n2o_proto](https://ws.n2o.dev/man/n2o_proto.htm) — N2O Loop
* [n2o_ring](https://ws.n2o.dev/man/n2o_ring.htm) — N2O Ring

## WebSocket

N2O Loop is directly connected and ran inside context of WebSocket handler.
Usually in Erlang we use `syn` or `gproc` OTP message buses.
As such buses are optional in MQTT setup we include bus drivers in WebSocket package.

* [n2o_ws](https://ws.n2o.dev/man/n2o_ws.htm) — N2O WebSocket Virtual Node
* [n2o_heart](https://ws.n2o.dev/man/n2o_heart.htm) — PING protocol
* [n2o_cowboy](https://ws.n2o.dev/man/n2o_cowboy.htm) — COWBOY API
* [n2o_gproc](https://ws.n2o.dev/man/n2o_gproc.htm) — GPROC bus backend
* [n2o_syn](https://ws.n2o.dev/man/n2o_syn.htm) — SYN bus backend

```sh
git clone git@github.com:synrc/sample && cd sample
rebar3 shell
open open http://localhost:8001/app/login.htm
```

## Protocols

N2O ships with 3 optional protocols.

* [n2o_ftp](https://ws.n2o.dev/man/n2o_ftp.htm) — N2O File protocol
* [n2o_heart](https://ws.n2o.dev/man/n2o_heart.htm) — N2O Heart protocol
* [nitro_n2o](https://nitro.n2o.dev/man/nitro_n2o.htm) — Nitrogen Web Framework protocol
* [bpe_n2o](https://bpe.n2o.dev) — Business Process Engine protocol

## Services

Formatters, Sessions, etc. Optional.

* [n2o_bert](https://ws.n2o.dev/man/n2o_bert.htm) — BERT encoder/decoder
* [n2o_json](https://ws.n2o.dev/man/n2o_json.htm) — JSON encoder/decoder
* [n2o_secret](https://ws.n2o.dev/man/n2o_secret.htm)  — AES/GCM-256 encoder/decoder
* [n2o_session](https://ws.n2o.dev/man/n2o_session.htm) — ETS session storage

## JavaScript

* [bert.js](https://ws.n2o.dev/man/bert.js.htm) — BERT encoder/decoder
* [utf8.js](https://ws.n2o.dev/man/utf8.js.htm) — UTF8 encoder/decoder
* [ieee754.js](https://ws.n2o.dev/man/ieee754.js.htm) — IEEE754 encoder/decoder
* [heart.js](https://ws.n2o.dev/man/heart.js.htm) — HEART protocol
* [ftp.js](https://ws.n2o.dev/man/ftp.js.htm)  — FTP protocol
* [n2o.js](https://ws.n2o.dev/man/n2o.js.htm) — N2O protocol loop
* [mq.js](https://ws.n2o.dev/man/mq.js.htm) — MQTT client

## Literature

* "N2O: no bullshit sane framework for wild web" [PDF](https://n2o.dev/books/n2o.pdf) (versions 0.11—4.4)
* "N2O BOOK Vol.2 Green Book" [HTML](https://n2o.dev/ua/books/vol.2/index.html) (versions 4.5—9.11)
