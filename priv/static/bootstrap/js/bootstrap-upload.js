(function($) {
  $.fn.upload = function(settings){
    var options = $.extend(true, {
      block_size: 10485760,
      beginUpload: function(){},
      deliverSlice: function(){},
      queryFile: function(){},
      complete: function(){} 
    }, settings);

    return this.filter('input[type="file"]').each(function(){return $.Upload(this, options)});
  };

  $.Upload = function(input, options){
    if (!(window.File && window.FileReader && window.FileList && window.Blob)) return false;

    var $input = $(input);
    var self = this;
    var pid;
    var reader;
    var block_size = options.block_size;
    var cancelled_upload;
    var paused_upload;
    var start_time;
    var file;
    var file_index = 0;
    var start_file_index;

    var info = $('<span/>').attr({class: 'info', name: 'info'});
    var browse_btn = $('<input type="button"/>').attr({class: 'btn btn-browse', value: 'Browse'}).on('click', function(e){$input.trigger('click');});
    var upload_btn = $('<input type="button"/>').attr({class: 'btn btn-upload', value: 'Upload'}).on('click', function(e){begin_upload();});
    var resume_btn = $('<input type="button"/>').attr({class: 'btn btn-resume', value: 'Resume'}).on('click', function(e){begin_upload();});
    var reupload_btn = $('<input type="button"/>').attr({class: 'btn btn-reupload', value: 'Reupload'}).on('click', function(e){file_index = 0;begin_upload();});
    var progress_bar = $('<div/>').attr({class:'bar'});
    var progress_label = $('<div/>').attr({class: 'progress-label'});
    var progress = $('<div/>').attr({class: 'progress progress-info'}).append(progress_bar, progress_label);
    var cancel_btn = $('<input type="button"/>').attr({class: 'btn btn-cancel', value: 'Cancel'})
      .on('click', function(e) {
        reset_upload();
        progress_label.html('');
        cancelled_upload=true;
    });
    var pause_btn = $('<input type="button"/>').attr({class: 'btn btn-pause', value: 'Pause'})
      .on('click', function (e) {
        paused_upload=true;
        cancel_btn.hide();
        pause_btn.hide();
        resume_btn.show();
        progress_label.html('');
      });

    $input.wrap("<div class='file_upload'></div>").hide().parent().append(
      progress, info, browse_btn, upload_btn, pause_btn, reupload_btn, resume_btn, cancel_btn
    );
    var file_buttons = $(':button', $input.parent());

    $input.on('change', function(e) {
      file = this.files[0];
      if(!file) return;
      info.html(file.name);
      progress_label.html('');
      file_buttons.hide();
      browse_btn.show();
      upload_btn.show();
      progress_bar.attr("style", "width: 0");
      options.queryFile(Bert.tuple(Bert.atom('query'),Bert.binary(file.name)));
    }).on('exist', function(e, fileSize){
      file_index = 0;
      var size = parseInt(fileSize);
      if (size>0) {
        file_index = size;
        file_buttons.hide();
        browse_btn.show();
        reupload_btn.show();
        if (file_index<file.size) {
          resume_btn.show();
          progress_label.html('Upload incomplete');
        } else {
          progress_label.html('File exists');
        }
        update_progress_bar();
      }
    }).on('begin_upload', function(e, pid){
      self.pid = pid;
      read_slice(file_index, block_size);
    }).on('upload', function(e, fileSize){
      var size = parseInt(fileSize);
      if (!paused_upload && !cancelled_upload && file_index<file.size) {
        read_slice(file_index, file_index + block_size);
      }
      if (paused_upload) progress_label.html('');
      if (cancelled_upload) {
        reset_upload();
        progress_label.html('');
      }
    }).on('error', function(e, msg){
      error(msg);
    });

    function reset_upload(){
      file_buttons.hide();
      browse_btn.show();
      cancelled_upload = false;
      paused_upload = false;
      info.html('');
      progress_label.html('');
      progress_bar.attr("style", "width: 0");
    }
    function error(message){ 
      reset_upload();
      progress_label.html(message);
    }
    function onabort(event){ error('File upload aborted'); reader.abort();}
    function onerror(event){
      switch(event.target.error.code) {
        case event.target.error.NOT_FOUND_ERR:    error('File not found');       break;
        case event.target.error.NOT_READABLE_ERR: error('File is not readable'); break;
        case event.target.error.ABORT_ERR:        error('File upload aborted');  break;
        default: error('An error occurred reading the file.');    };
    }
    function onloadend(event){
      if(event.target.readyState == FileReader.DONE){
        if (options.deliverSlice(Bert.tuple(Bert.atom('upload'), Bert.binary(self.pid), Bert.binary(event.target.result))) == false) {
          error('Error delivering data to server');
          return;
        }
        file_index += block_size;
        calculate_eta();
        update_progress_bar();

        if (file_index>=file.size){
          file_buttons.hide();
          browse_btn.show();
          progress_label.html('Upload complete');
          options.complete(Bert.tuple(Bert.atom('complete'), Bert.binary(self.pid)));
        }
      }
    }

    function calculate_eta()  {
      var delta_ms = Date.now() - start_time;
      var rate = (file_index- start_file_index) / delta_ms;
      var remaining_ms = (file.size - file_index) / rate;
      var delta_hr = parseInt(Math.floor(remaining_ms/3600000));
      remaining_ms -= delta_hr*3600000;
      var delta_min = parseInt(Math.floor(remaining_ms/60000));
      remaining_ms -= delta_min*60000;
      var delta_sec = parseInt(Math.floor(remaining_ms/1000));
      if (delta_sec>10) delta_sec = parseInt(Math.floor(delta_sec/10)*10);
      var eta = "";
      if (delta_sec>0) eta = delta_sec + " secs";
      if (delta_min>0) eta = delta_min + " mins";
      if (delta_hr>0) eta = delta_hr + " hours";
      if (delta_ms>5000) progress_label.html(eta);
    }
    function update_progress_bar()  {
      var progress = Math.floor(100* (file_index / file.size));
      progress_bar.attr("style", "width: "+ progress + "%");
    }

    function read_slice(start, end)   {
      reader = new FileReader();
      reader.onabort = onabort;
      reader.onerror = onerror;
      reader.onloadend = onloadend;
      var blob = file.slice(start, end);
      reader.readAsBinaryString(blob);
    }

    function begin_upload()   {
      file_buttons.hide();
      pause_btn.show();
      cancel_btn.show();
      progress_label.html('');
      start_time = Date.now();
      start_file_index = file_index;
      if (paused_upload) paused_upload = false;
      options.beginUpload(Bert.tuple(Bert.atom('begin_upload'), Bert.binary(file.name)));
    }

    reset_upload();
    return this;
  };

})(window.jQuery || window.Zepto);
