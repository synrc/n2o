try { module.exports = {ftp:ftp}; } catch (e) { }

// N2O File Transfer Protocol

var ftp = {
    $file: undefined, $reader: undefined, $block: undefined,
    init: function(file, force) { this.$file = file; this.send('', 'init', 1); },
    start: function() { this.send_slice(0, this.$block); },
    send: function(data, status, force) {
        ws.send(enc(tuple(atom('ftp'),number(1), bin(this.$file.name), number(3),number(4),number(5),number(6),
        number(7),bin(data),bin(status||'send'),number(force || data.byteLength),number(11)))); },
    send_slice: function(start, end) {
        this.$reader = new FileReader();
        this.$reader.onloadend=function(e) {
             var res=e.target, data=e.target.result;
             if(res.readyState==FileReader.DONE&&data.byteLength>0) ftp.send(data); };
        this.$reader.readAsArrayBuffer(this.$file.slice(start,end)); } }

$file.do = function(rsp) {
    var offset = rsp.v[6].v, block = rsp.v[10].v, status = rsp.v[9].v;
    switch (status) { case 'init': console.log('Block: '+block); ftp.$block = block; break;
                      case 'send': console.log('Offset: '+offset); if(block>0) ftp.send_slice(offset, offset+block); } }

// NITRO Upload Element

function Upload(id, options) {
    if (!(window.File && window.FileReader && window.FileList && window.Blob)) return false;

    var $input = document.querySelector(id);
    var self = this;
    var pid, reader, file;

    var hide_btns = function(){for(var i=0; i< file_btns.length;++i){file_btns[i].style.display='none';}};
    var create_btn= function(title,html, clazz) {
        var btn = document.createElement('a');btn.setAttribute('href', '#');btn.title=title;btn.innerHTML=html;
        var i = document.createElement('i'); i.className=clazz;
        btn.appendChild(i);
        return btn; };
    var create_el       = function(tag, clazz) { var el = document.createElement(tag);el.className=clazz;return el; };
    var append_children = function(p, ch) { for(var i=0; i < ch.length;i++){p.appendChild(ch[i]);}; };

    function updateButtons(file) {
        browse_btn.style.display='inline-block';
        upload_btn.style.display='inline-block';
        reload_btn.style.display='none';
        ftp.init(file,1);
    }

    $input.addEventListener('change',function(e){
        file = this.files[0];
        if(!file) return;
        info.innerHTML=file.name;
        hide_btns();
        updateButtons(file);
    });

    var browse_btn = create_btn('browse',  'browse',  '1'); browse_btn.addEventListener('click', function(e){$input.click(); e.preventDefault();}, false);
    var upload_btn = create_btn('upload',  'upload',  '1'); upload_btn.addEventListener('click', function(e){ftp.start();}, false);
    var reload_btn = create_btn('reupload','reupload','1'); reload_btn.style.display='none';

    var info = create_el('span', 'info');
    var ctl  = create_el('span', 'ctl');
    var fu   = create_el('div',   options.cid); fu.setAttribute('contenteditable', false);

    append_children(ctl, [browse_btn, upload_btn, reload_btn]);
    append_children(fu,  [info,ctl])

    $input.parentNode.insertBefore(fu, $input);

    var file_btns = fu.querySelectorAll("a");
}
