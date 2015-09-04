
// JSON formatter

var $client = {};
$client.on = function onclient(evt, callback)
{
    try {  msg = JSON.parse(evt.data);

           if (typeof callback == 'function' && msg.data) callback(msg.data);
           if (debug) console.log(msg.eval);
           if (msg.eval) try { eval(msg.eval); }
                   catch (e) { return { status: "error", desc: e }; }

    } catch (ex) { return { status: "error", desc: ex }; }

    return { status: "ok" };
};
