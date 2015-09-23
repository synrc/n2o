try { module.exports = {ftp:ftp}; } catch (e) { }

// N2O File Transfer Protocol

var ftp = {
    $file: undefined, $reader: undefined, $block: undefined, $offset: undefined,
    init: function(file, force) { ftp.$file = file; ftp.send('', 'init', 1); },
    start: function() { ftp.$active = true; ftp.send_slice(ftp.$offset, ftp.$offset + ftp.$block); },
    stop: function() { ftp.$active = false; },
    send: function(data, status, force) {
        ws.send(enc(tuple(atom('ftp'),number(1), bin(ftp.$file.name), number(3),number(4),number(5),number(6),
        number(ftp.$file.size),bin(data),bin(status||'send'),number(force || data.byteLength),number(11)))); },
    send_slice: function(start, end) {
        this.$reader = new FileReader();
        this.$reader.onloadend=function(e) {
             var res=e.target, data=e.target.result;
             if(res.readyState==FileReader.DONE&&data.byteLength>0) ftp.send(data); };
        this.$reader.readAsArrayBuffer(ftp.$file.slice(start,end)); } }

$file.do = function(rsp) {
    var offset = rsp.v[6].v, block = rsp.v[10].v, status = rsp.v[9].v;
    switch (status) { case 'init': ftp.$offset = offset; ftp.$block = block; break;
                      case 'send': var x = qi('ftp_status'); if(x) x.innerHTML = offset;
                                   if(block>0 && ftp.$active) ftp.send_slice(offset, offset+block); } }
