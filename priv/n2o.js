var msg = 0;
var ws;
var utf8 = {};

//WebSocket = undefined; // test XHR fallback

function addStatus(text){
    var date = new Date();
    if (document.getElementById('n2ostatus')) {
        document.getElementById('n2ostatus').innerHTML =
            document.getElementById('n2ostatus').innerHTML + "E> " + text + "<br/>";
    }
}

utf8.toByteArray = function(str) {
    var byteArray = [];
    if (str !== undefined && str !== null)
    for (var i = 0; i < str.length; i++)
        if (str.charCodeAt(i) <= 0x7F)
            byteArray.push(str.charCodeAt(i));
        else {
            var h = encodeURIComponent(str.charAt(i)).substr(1).split('%');
            for (var j = 0; j < h.length; j++)
                byteArray.push(parseInt(h[j], 16));
        }
    return byteArray;
};

function byteArray8toString (byteArray, separator) {
    if (typeof byteArray == 'undefined' || byteArray.byteLength == 0) { return "" };
    separator = typeof separator !== 'undefined' ? separator : ',';
    var dataView = new DataView(byteArray);
    var s = dataView.getInt8(0).toString();
    for (var i = 1; i < byteArray.byteLength; i++)
        s = s + separator + dataView.getInt8(i).toString();
    return s;
}

function WebSocketsInit(){
    if ("MozWebSocket" in window) { WebSocket = MozWebSocket; }
    if ("WebSocket" in window) {
        ws = new bullet("ws://"+
          (null == transition.host ? window.location.hostname : transition.host)
               + ":"+ (null == transition.port ? window.location.port : transition.port)
             + "/ws" + window.location.pathname + window.location.search);
        initialized = false;
        ws.onmessage = function (evt) {

            try { // try to parse JSON envelop {eval:"",data:""}

                msg = JSON.parse(evt.data);

                if (typeof handle_web_socket == 'function' && msg.data) { // Data
//                    addStatus("Received: " + bert.decodebuf(msg.data));
                    handle_web_socket(msg.data);
                }

                if (msg.eval) { // Eval
//                    addStatus("Evaluate: " + msg.eval);
                    try{eval(msg.eval);}catch(e){console.log(e); console.log(msg.eval);};
                }

            } catch (ex) { // try to parse known binary formats

//                console.log("JSON parsing failed: " + ex);
//                console.log("MessageEvent: ");
//                console.log(evt.data);

                var header_reader = new FileReader();
                header_reader.addEventListener("loadend", function() {

                    
                    header_view = new DataView(header_reader.result);
                    
                    try { // BERT encoding

                        if (header_view.getUint8(0) !== 131) { throw ("Not a valid BERT header."); }
                        else {
                            var bert_reader = new FileReader();
                            bert_reader.addEventListener("loadend", function() {
                                var erlang = dec(bert_reader.result);
                                if (typeof handle_web_socket == 'function')
                                    handle_web_socket(erlang);
                                else console.log("Raw BERT Received: " + erlang);
                            });
                            bert_reader.readAsArrayBuffer(evt.data);
                        }

                    } catch (x) { // Unknown Binaries

                        //debugger;
                        
                        var meta_offset = 12
                        
                        if (header_view.getUint8(0) == 66 && header_reader.result.byteLength == meta_offset) {
                            var id = header_view.getUint32(1);
                            var type = header_view.getUint8(5);
                            var app = header_view.getUint8(6);
                            var version = header_view.getUint8(7);
                            var meta_length = header_view.getUint32(8);
                            var meta_reader = new FileReader();
                            var data_offset = meta_offset + meta_length;
                            meta_reader.addEventListener("loadend", function() {
                                if (typeof handle_web_socket_blob_with_header == 'function')
                                    handle_web_socket_blob_with_header(id, type, app, version, meta_reader.result, evt.data.slice(data_offset));
                                else {
                                    //debugger;
                                    console.log("Raw Binary With Header Received: Header [" + byteArray8toString(header_reader.result)
                                        + "] Meta [" + byteArray8toString(meta_reader.result)
                                        + "] Data lehgth: " + (evt.data.size - data_offset));
                                }
                            });
                            meta_reader.readAsArrayBuffer(evt.data.slice(meta_offset, data_offset));
                        }
                        else {
                            
                            if (typeof handle_web_socket_blob == 'function')
                                handle_web_socket_blob(evt.data);
                            else {
                                var reader = new FileReader();
                                reader.addEventListener("loadend", function() {
                                    if (reader.result.byteLength > 0) {
                                        var dataView = new DataView(reader.result);
                                        var s = dataView.getInt8(0).toString();
                                        for (var i=1;i<reader.result.byteLength;i++)
                                            s = s + "," + dataView.getInt8(i).toString();
                                        console.log("Unknown Raw Binary Received: [" + s + "]");
                                    }
                                });
                                reader.readAsArrayBuffer(evt.data);
                            }
                        }
                    }

                });
//                console.log(evt.data);
                header_reader.readAsArrayBuffer(evt.data.slice(0, 12));

            }

        };
        ws.onopen = function() { if (!initialized) { ws.send(['N2O', transition.pid]); initialized = true; } };
        ws.onclose = function() { addStatus("websocket was closed"); };
    } else {
        addStatus("sorry, your browser does not support websockets.");
    }
}

WebSocketsInit();

