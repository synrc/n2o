
// JSON formatter

var $client = {};
$client.on = function onclient(evt, callback) {
    try {  msg = JSON.parse(evt.data);
           if (debug) console.log(JSON.stringify(msg));
           if (typeof callback == 'function' && msg) callback(msg);
           for (var i=0;i<$bert.protos.length;i++) {
                p = $bert.protos[i]; if (p.on(msg, p.do).status == "ok") return { status: "ok"}; }
    } catch (ex) { return { status: "error" }; }
    return { status: "ok" }; };
