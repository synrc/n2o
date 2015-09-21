var ftp = {
  $file : undefined,
  $reader: undefined,
  init: function(file, force){
    this.$file = file;
    this.send('', 'init', 1);
  },
  send : function(data, status, force){
    console.log(status);
    console.log(data);
    var s = tuple(atom('ftp'),number(1), bin(this.$file.name),
          number(3),number(4),number(5),number(6),
          number(7),bin(data),bin(status||'send'),number(force || data.byteLength),number(11));
    console.log(s);
    ws.send(enc(s));
  }
}

function Upload(id, options){
  if (!(window.File && window.FileReader && window.FileList && window.Blob)) return false;
  var $input = document.querySelector(id);
  var self = this;
  var pid, reader, file;
  var block_size = 1024;
  var hide_btns = function(){for(var i=0; i< file_btns.length;++i){file_btns[i].style.display='none';}};
  var create_btn= function(title,html, clazz){
    var btn = document.createElement('a');btn.setAttribute('href', '#');btn.title=title;btn.innerHTML=html;
    var i = document.createElement('i'); i.className=clazz;
    btn.appendChild(i);
    return btn;};
  var create_el = function(tag, clazz){
    var el = document.createElement(tag);el.className=clazz;return el;};
  var append_children = function(p, ch){for(var i=0; i < ch.length;i++){p.appendChild(ch[i]);};};

  $input.addEventListener('change',function(e){
    file = this.files[0];
    if(!file) return;

    ftp.init(file);

    info.innerHTML=file.name;
    hide_btns();

    browse_btn.style.display='inline-block';
    upload_btn.style.display='inline-block';
    reload_btn.style.display='none';
  });

  var browse_btn = create_btn('browse', 'browse', '1');
  browse_btn.addEventListener('click', function(e){$input.click(); e.preventDefault();}, false);

  var upload_btn = create_btn('upload', 'upload', '1');
  upload_btn.onclick=function(){ read_slice(0, 1024);  };

  var reload_btn = create_btn('reupload', 'reupload', '1');
  reload_btn.style.display='none';

  var info = create_el('span', 'info');

  var ctl = create_el('span', 'ctl');
  append_children(ctl, [browse_btn, upload_btn, reload_btn]);

  var fu = create_el('div', options.cid); fu.setAttribute('contenteditable', false);
  append_children(fu, [info,ctl])

  $input.parentNode.insertBefore(fu, $input);

  var file_btns = fu.querySelectorAll("a");

  var read_slice = function(start, end) {
    console.log('-> read slice');
    reader = new FileReader();
    reader.onloadend = function(e){
      if(e.target.readyState == FileReader.DONE && e.target.result.byteLength > 0){
        ftp.send(e.target.result);
      }
    };
    reader.readAsArrayBuffer(ftp.$file.slice(start, end));
  };

  $file.do = function(rsp){
    console.log(rsp);
    var offset = rsp.v[6].v;
    var block = rsp.v[10].v;
    console.log('status: ' + rsp.v[9].v);
    switch(rsp.v[9].v){
      case 'init': break;
      case 'exist':
        reload_btn.style.display='inline-block';
        upload_btn.style.display='none';
        reload_btn.onclick=ftp.init(file, 1);
        break;
      default:
         console.log('offset: ' + offset);
         console.log('block: ' + block);
        if(block>0) read_slice(offset, offset + block);
    }
  }
}
