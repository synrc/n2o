try { module.exports = {ftp:ftp}; } catch (e) { }

// N2O File Transfer Protocol

var ftp = {
    init:  function(file) { ftp.file=file; ftp.send('','init',1); },
    start: function() { ftp.active = true; ftp.send_slice(ftp.offset, ftp.offset + ftp.block); },
    stop:  function() { ftp.active = false; },
    send:  function(data, status, block) {
        ws.send(enc(tuple(atom('ftp'),bin(co(session)),bin(ftp.filename || ftp.file.name),
            ftp.meta?ftp.meta:bin(''),number(ftp.file.size),number(ftp.offset || 0),
            number(block || data.byteLength),bin(data),bin(status||'send')))); },
    send_slice: function(start, end) {
        this.reader = new FileReader();
        this.reader.onloadend=function(e) { var res=e.target, data=e.target.result;
             if(res.readyState==FileReader.DONE&&data.byteLength>0) ftp.send(data); };
        this.reader.readAsArrayBuffer(ftp.file.slice(start,end)); } }

$file.do = function(rsp) {
    var offset = rsp.v[5].v, block = rsp.v[6].v, status = utf8_dec(rsp.v[8].v);
    switch (status) {
        case 'init': ftp.offset = offset; ftp.block = block; if(ftp.autostart) ftp.start(); break;
        case 'send': var x = qi('ftp_status'); if(x) x.innerHTML = offset;
           if(block>0 && ftp.active) ftp.send_slice(offset, offset+block); break;
        case 'relay': if (typeof ftp.relay === 'function') ftp.relay(rsp); break;
    }
};
