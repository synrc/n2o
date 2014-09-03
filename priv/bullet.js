
// N2O Bullet

function bullet(url) {

    var CONNECTING = 0;
    var OPEN = 1;
    var CLOSING = 2;
    var CLOSED = 3;

    var transports = {

        websocket: function() {
            var transport = null;
            if (window.WebSocket) { transport = window.WebSocket; }
            if (transport) { return {'heart': true, 'transport': transport}; }
            return null;
        },

        xhrPolling: function() {
            var timeout;
            var xhr;

            nextPoll();

            var fake = {
                readyState: CONNECTING,

                receive: function(data) {
                    if (fake.readyState == CONNECTING) { fake.readyState = OPEN; fake.onopen(fake); }
                    if (data.length != 0) { fake.onmessage({'data': data }); }
                    if (fake.readyState == OPEN) { nextPoll(); }
                },

                send: function(data) {
                    if (this.readyState != CONNECTING && this.readyState != OPEN) return false;
                    var fakeurl = url.replace('ws:', 'http:').replace('wss:', 'https:');
                    var request = new XMLHttpRequest();
                    request.open('POST',fakeurl,true);
                    request.setRequestHeader('Content-Type',
                        'application/x-www-form-urlencoded; charset=utf-8');
                    request.setRequestHeader('X-Socket-Transport','xhrPolling');
                    request.onload = function() { fake.receive(request.response); };
                    request.send(data);
                    return true;
                },
                close: function(){
                    this.readyState = CLOSED;
                    xhr.abort();
                    clearTimeout(timeout);
                    fake.onclose();
                },
                onopen: function(){},
                onmessage: function(){},
                onerror: function(){},
                onclose: function(){}
            };

            function poll(pooling){
                var fakeurl = url.replace('ws:', 'http:').replace('wss:', 'https:');
                var request = new XMLHttpRequest();
                request.open('GET',fakeurl,true);
                request.setRequestHeader('Content-Type',
                    'application/x-www-form-urlencoded; charset=utf-8');
                request.setRequestHeader('X-Socket-Transport','xhrPolling');
                request.onload = function() { fake.receive(request.response); };
                request.onerror = function() { fake.onerror(); };
                request.send({});
            }

            function nextPoll() { timeout = setTimeout(function(){poll();}, 1000); }


            return {'heart': false, 'transport': function(){ return fake; fake.nextPoll(); }};
        }
    };

    var tn = 0;
    function next() {
        var c = 0;

        for (var f in transports) {
            if (tn == c) {
                var t = transports[f]();
                if (t) { var ret = new t.transport(url); ret.heart = t.heart; return ret; }
                tn++;
            }
            c++;
        }
        return false;
    }

    var stream = new function() {
        var isClosed = true;
        var readyState = CLOSED;
        var heartbeat;
        var delay = 80;
        var delayDefault = 80;
        var delayMax = 10000;

        var transport;
        function init() {

            isClosed = false;
            readyState = CONNECTING;
            transport = next();

            if (!transport) {
                delay = delayDefault;
                tn = 0;
                stream.ondisconnect();
                setTimeout(function(){init();}, delayMax);
                return false;
            }

            transport.onopen = function() {
                delay = delayDefault;

                if (transport.heart) { 
                    heartbeat = setInterval(function(){stream.onheartbeat();}, 4000);
                }

                if (readyState != OPEN) { readyState = OPEN; stream.onopen(); }
            };

            transport.onclose = function() {
                if (isClosed) { return; }

                transport = null;
                clearInterval(heartbeat);

                if (readyState == CLOSING){
                    readyState = CLOSED;
                    stream.onclose();
                } else {
                    if (readyState == CONNECTING) tn++;
                    delay *= 2;
                    if (delay > delayMax) { delay = delayMax; }
                    isClosed = true;
                    setTimeout(function() { init(); }, delay);
                }
            };
            transport.onerror = transport.onclose;
            transport.onmessage = function(e){
            stream.onmessage(e);
            };
        }

        init();

        this.onopen = function(){};     this.oninit = function(){};
        this.onmessage = function(){};  this.ondisconnect = function(){ initialized = false; };
        this.onclose = function(){};    this.onheartbeat = function(){ return this.send('PING'); };

        this.setURL = function(newURL) { url = newURL; };
        this.send = function(data){
            if (transport) return transport.send(data); else return false;
        };
        this.close = function(){
            readyState = CLOSING;
            if (transport) transport.close();
        };
    };

    return stream;
}
