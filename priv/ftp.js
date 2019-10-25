
// N2O File Transfer Protocol

function uuid() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
  var r = Math.random()*16|0, v = c == 'x' ? r : (r&0x3|0x8); return v.toString(16); });
}

var ftp = {
    queue: [],
    init: function (file) {
        var item = {
            id: uuid(),
            status: 'init',
            autostart: ftp.autostart || false,
            name: ftp.filename || file.name,
            sid: ftp.sid || token(), // co(session),
            meta: ftp.meta || bin(client()),
            offset: ftp.offset || 0,
            block: 1,
            total: file.size,
            file: file
        };
        ftp.queue.push(item);
        ftp.send(item, '', 1);
        return item.id;
    },
    start: function (id) {
        if (ftp.active) { id && (ftp.item(id).autostart = true); return false; }
        var item = id ? ftp.item(id) : ftp.next();
        if (item) { ftp.active = true; ftp.send_slice(item); }
    },
    stop: function (id) {
        var item = ftp.item(id);
        var index = ftp.queue.indexOf(item);
        ftp.queue.splice(index, 1);
        ftp.active = false;
        ftp.start();
    },
    send: function (item, data) {
        ws.send(enc(tuple(atom('ftp'),
            bin(item.id),
            bin(item.sid),
            bin(item.name),
            item.meta,
            number(item.total),
            number(item.offset),
            number(item.block || data.byteLength),
            bin(data),
            bin(item.status || 'send')
        )));
    },
    send_slice: function (item) {
        this.reader = new FileReader();
        this.reader.onloadend = function (e) {
            var res = e.target, data = e.target.result;
            if (res.readyState === FileReader.DONE && data.byteLength >= 0) {
                ftp.send(item, data);
            }
        };
        this.reader.readAsArrayBuffer(item.file.slice(item.offset, item.offset + item.block));
    },
    item: function (id) { return ftp.queue.find(function (item) { return item && item.id === id; }); },
    next: function () { return ftp.queue.find(function (next) { return next && next.autostart }); }
};

$file.progress = function onprogress(offset,total) {
    var x = qi('ftp_status'); if (x) x.innerHTML = offset;
};

$file.do = function (rsp) {
    var total = rsp.v[5].v, offset = rsp.v[6].v, block = rsp.v[7].v, status = utf8_arr(rsp.v[9].v);
    switch (status) {
        case 'init':
            if(block == 1) return;
            var item = ftp.item(utf8_arr(rsp.v[1].v)) || '0';
            item.offset = offset;
            item.block = block;
            item.name = utf8_arr(rsp.v[3].v);
            item.status = undefined;
            if (item.autostart) ftp.start(item.id);
            break;
        case 'send':
            $file.progress(offset,total);
            var item = ftp.item(utf8_arr(rsp.v[1].v));
            item.offset = offset;
            item.block = block;
            (block > 0 && ftp.active) ? ftp.send_slice(item) : ftp.stop(item.id)
            break;
        case 'relay': debugger; if (typeof ftp.relay === 'function') ftp.relay(rsp); break;
    }
};
