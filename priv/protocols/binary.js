var $binary = {};
$binary.on = function onbinary(evt, callback) //
{
    // console.log("Binary On");
    var HEAD_SIZE = 36;
    // Check for FileReader.readAsArrayBuffer()
    if(Blob.prototype.isPrototypeOf(evt.data) && evt.data.length >= HEAD_SIZE) {
        var reader = new FileReader();
        reader.addEventListener("loadend", function() {
            var header_view = new DataView(reader.result);
            var head_char = header_view.getUint8(0);
    
            if (head_char !== 132 || reader.result.byteLength != HEAD_SIZE) 
                  return { status: "error", desc: "not #binary{}" };
    
            // Headered Binaries
            // [132, HEADER, META, DATA]
            var id = header_view.getUint32(1);
            var type = header_view.getUint8(5);
            var app = header_view.getUint8(6);
            var version = header_view.getUint8(7);
            var from = header_view.getUint32(8);
            var to = header_view.getUint32(12);
            var user1 = header_view.getFloat64(16);
            var user2 = header_view.getFloat64(24);
            var data_offset = HEAD_SIZE + header_view.getUint32(32);
            var meta_reader = new FileReader();
    
            meta_reader.addEventListener("loadend", function() {
                if (typeof callback == 'function')
                      callback(id, type, app, version, from, to, user1, user2,
                         meta_reader.result, evt.data.slice(data_offset));
    
                else {
                     console.log("Raw Binary With Header Received: Header [" + 
                          utf8_fromByteArray(reader.result) + "] Meta [" + 
                          utf8_fromByteArray(meta_reader.result) + "] Data lehgth: " +
                           (evt.data.size - data_offset));
    
                     console.log("Header fields { id: " + id + ", type: " + type + ", app: " + 
                          app + ", version: " + version + ", from: " + from + ", to: " + to 
                              + ", user1: " + user1 + ", user2: " + user2 + " }");
                }
            });
      
            meta_reader.readAsArrayBuffer(evt.data.slice(HEAD_SIZE, data_offset));
    
            if (typeof callback  == 'function') callback(erlang);
        });
        reader.readAsArrayBuffer(evt.data.slice(0, HEAD_SIZE)); 
        return { status: "ok" };
    } else { return { status: "error", desc: "not #binary{}" }; }
};
