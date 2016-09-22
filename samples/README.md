N2O: Erlang Web Framework on WebSockets
=======================================

![ScreenShot](http://synrc.com/images/n2o_sample_screen.png)

Samples
-------

Samples provided as Erlang release packaged
with *web* Erlang application which contains modules:

* REST samples
* N2O samples
* XEN support

Prerequisites
-------------

* erlang
* mad
* inotify-tools (Linux, for filesystem watching)
* uglify (assets)

Run
---

To run just perform on Windows, Linux and Mac

    $ ./mad deps compile plan repl

On BSD you should use gmake

And open it in browser [http://localhost:8000](http://localhost:8000)
If you want to try pure Single Page Application (SPA) wich
connects to Erlang N2O Application Server you should use
[http://localhost:8000/static/spa/spa.htm](http://localhost:8000/static/spa/spa.htm)

For full features of make please refer to [https://github.com/synrc/otp.mk](https://github.com/synrc/otp.mk)

Production Assets
-----------------

    $ ./mad static min
    > application:set_env(n2o,mode,prod).

Xen
---

To run on Xen is a bit tricky:

    $ sudo apt-get install xen-hypervisor-amd64
    $ echo XENTOOLSTACK=xl > /etc/default/xen

Boot into Xen 4.2 Domain-0 and create network bridge:

    $ sudo brctl addbr docker0
    $ sudo ip addr add 172.16.42.1/24 dev docker0

Compile Image at Erlang on Xen builder:

    $ rebar get-deps compile
    $ ./nitrogen_static.sh
    $ rebar ling-build-image
    $ sudo xl create -c xen.config

Inside Ling start n2o_sample application:

    Ling 0.2.2 is here
    Started in 49438 us
    Erlang [ling-0.2.2]

    Eshell V5.10.2  (abort with ^G)
    1> application:start(n2o_sample).

And open it in browser [http://172.16.42.108:8000](http://172.16.42.108:8000)

Credits
-------

* Maxim Sokhatsky

OM A HUM
