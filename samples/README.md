N2O: Erlang Web Framework on WebSockets
=======================================

Samples
-------

Samples provided as Erlang release packaged
with *web* Erlang application which contains modules:

* REST samples
* N2O samples

Run
---

To run no fancy scripts needed, just pure rebar

    $ rebar get-deps
    $ rebar compile
    $ cd rels/web && rebar -f generate
    $ node/bin/node console

And open it in browser [http://localhost:8000](http://localhost:8000)
