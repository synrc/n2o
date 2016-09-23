try { module.exports = {http:http}; } catch (e) { }

// Template: http.send(url + '?' + 'test1=1&test2=2', 'GET', '', {SomeHeader:'some header'}).done(function(data, status, headers) {console.log(data););

var http = {
    receiveData:null,
    doneCallback:function(){},
    failCallback:function(){},
    settings:{},
    send: function(settings) {
        var $ = this;
        var defSett = {
            url:window.location.href,
            method:'GET',
            body:'',
            headers:{},
            returnType:'html',
        };
        $.settings = {};
        if (typeof settings === 'string') {
            $.settings = $.merge(defSett, {url:settings});
        } else {
            $.settings = $.merge(defSett, settings);
        }
        if (/^\/\//.test($.settings.url)) {
            $.settings.url = window.location.protocol + $.settings.url
        }
        if (!/^http[s]*\:\/\//.test($.settings.url)) {
            $.settings.url = window.location.origin + $.settings.url
        }
        var tList = [];
        if ($.settings.headers) {
            for (var prop in $.settings.headers) {
                tList.push(tuple(bin(prop),bin($.settings.headers[prop])));
            }
        }
        ws.send(enc(tuple(atom('http'),
            bin($.settings.url),
            bin($.settings.method||'GET'),
            bin($.settings.body||''),
            tList)
        ));
        return $;
    },
    back: function(d, s, h) {
        if (s >= 400) {
            this.failCallback(d, s, h);
        } else {
            if (this.settings.returnType === 'json') {
                try {
                    d = JSON.parse(this.escapeSpecialChars(d));
                } catch (e) {
                    if (debug) console.error('Unexpected string for JSON');
                };
            }
            this.doneCallback(d, s, h);
        }
    },
    done: function(callback) {
        this.doneCallback = callback;
        return this;
    },
    fail: function(callback) {
        this.failCallback = callback;
        return this;
    },
    merge: function(obj, src) {
        var newO = {};
        Object.keys(obj).forEach(function(key){newO[key] = obj[key];});
        Object.keys(src).forEach(function(key){newO[key] = src[key];});
        return newO;
    },
    escapeSpecialChars: function(jsonString) {
        return jsonString.replace(/\n/g, "\\n")
            .replace(/\r/g, "\\r")
            .replace(/\t/g, "\\t")
            .replace(/\f/g, "\\f");
    }
};