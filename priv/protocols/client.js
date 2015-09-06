
// JSON formatter

var $client = {};
$client.on = function onclient(evt, callback) {
    try {  msg = JSON.parse(evt.data);
           if (debug) console.log(evt);
           if (typeof callback == 'function' && msg.data) callback(msg.data);
           if (msg.eval) try { eval(msg.eval); }
                   catch (e) { return { status: "error", desc: e }; }
    } catch (ex) { return { status: "error", desc: "json" }; }
    return { status: "ok" }; };
