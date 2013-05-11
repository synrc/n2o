/*
	Copyright (c) 2011-2012, Loïc Hoguin <essen@ninenines.eu>

	Permission to use, copy, modify, and/or distribute this software for any
	purpose with or without fee is hereby granted, provided that the above
	copyright notice and this permission notice appear in all copies.

	THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
	WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
	MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
	ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
	WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
	ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
	OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/

/**
	Bullet is a client-side javascript library AND server-side Cowboy handler
	to manage continuous streaming. It selects the proper transport in a fully
	automated way and makes sure to always reconnect to the server on any
	disconnect. You only need to handle sending messages, receiving them,
	and managing the heartbeat of the stream.

	Usage: $.bullet(url);

	Then you can register one of the 4 event handlers:
	onopen, onmessage, onclose, onheartbeat.

	onopen is called once right after starting the bullet stream.
	onmessage is called once for each message receveid.
	onclose is called once right after you voluntarily close the socket.
	onheartbeat is called once every few seconds to allow you to easily setup
	a ping/pong mechanism.
*/
(function($){$.extend({bullet: function(url){
	var CONNECTING = 0;
	var OPEN = 1;
	var CLOSING = 2;
	var CLOSED = 3;

	var transports = {
		/**
			The websocket transport is disabled for Firefox 6.0 because it
			causes a crash to happen when the connection is closed.
			@see https://bugzilla.mozilla.org/show_bug.cgi?id=662554
		*/
		websocket: function(){
			var transport = null;

			if (window.WebSocket){
				transport = window.WebSocket;
			}

			if (window.MozWebSocket
					&& navigator.userAgent.indexOf("Firefox/6.0") == -1){
				transport = window.MozWebSocket;
			}

                       transport.binaryType = "arrayuffer";

			if (transport){
				return {'heart': true, 'transport': transport};
			}

			return null;
		},

		xhrPolling: function(){
			var timeout;
			var xhr;

			var fake = {
				readyState: CONNECTING,
				send: function(data){
					if (this.readyState != CONNECTING && this.readyState != OPEN){
						return false;
					}

					var fakeurl = url.replace('ws:', 'http:').replace('wss:', 'https:');

					$.ajax({
						async: false,
						cache: false,
						type: 'POST',
						url: fakeurl,
						data: data,
						dataType: 'text',
						contentType:
							'application/x-www-form-urlencoded; charset=utf-8',
						headers: {'X-Socket-Transport': 'xhrPolling'},
						success: function(data){
							if (data.length != 0){
								fake.onmessage({'data': data});
							}
						}
					});

					return true;
				},
				close: function(){
					this.readyState = CLOSED;
					xhr.abort();
					clearTimeout(timeout);
					fake.onclose();
				},
				onopen: function(){},
				onmessage: function(){},
				onerror: function(){},
				onclose: function(){}
			};

			function poll(){
				var fakeurl = url.replace('ws:', 'http:').replace('wss:', 'https:');

				xhr = $.ajax({
					type: 'GET',
					cache: false,
					url: fakeurl,
					dataType: 'text',
					data: {},
					headers: {'X-Socket-Transport': 'xhrPolling'},
					success: function(data){
						if (fake.readyState == CONNECTING){
							fake.readyState = OPEN;
							fake.onopen(fake);
						}
						// Connection might have closed without a response body
						if (data.length != 0){
							fake.onmessage({'data': data});
						}
						if (fake.readyState == OPEN){
							nextPoll();
						}
					},
					error: function(xhr){
						fake.onerror();
					}
				});
			}

			function nextPoll(){
				timeout = setTimeout(function(){poll();}, 100);
			}

			nextPoll();

			return {'heart': false, 'transport': function(){ return fake; }};
		}
	};

	var tn = 0;
	function next(){
		var c = 0;

		for (var f in transports){
			if (tn == c){
				var t = transports[f]();
				if (t){
					var ret = new t.transport(url);
					ret.heart = t.heart;
					return ret;
				}

				tn++;
			}

			c++;
		}

		return false;
	}

	var stream = new function(){
		var isClosed = true;
		var readyState = CLOSED;
		var heartbeat;
		var delay = 80;
		var delayDefault = 80;
		var delayMax = 10000;

		var transport;
		function init(){

			isClosed = false;
			readyState = CONNECTING;
			transport = next();

			if (!transport){
				// Hard disconnect, inform the user and retry later
				delay = delayDefault;
				tn = 0;
				stream.ondisconnect();
				setTimeout(function(){init();}, delayMax);
				return false;
			}

			transport.onopen = function(){
				// We got a connection, reset the poll delay
				delay = delayDefault;

				if (transport.heart){
					heartbeat = setInterval(function(){stream.onheartbeat();}, 20000);
				}

				if (readyState != OPEN){
					readyState = OPEN;
					stream.onopen();
				}
			};
			transport.onclose = function(){
				// Firefox 13.0.1 sends 2 close events.
				// Return directly if we already handled it.
				if (isClosed){
					return;
				}

				transport = null;
				clearInterval(heartbeat);

				if (readyState == CLOSING){
					readyState = CLOSED;
					stream.onclose();
				} else{
					// Close happened on connect, select next transport
					if (readyState == CONNECTING){
						tn++;
					}

					delay *= 2;
					if (delay > delayMax){
						delay = delayMax;
					}

					isClosed = true;

					setTimeout(function(){
						init();
					}, delay);
				}
			};
			transport.onerror = transport.onclose;
			transport.onmessage = function(e){
				stream.onmessage(e);
			};
		}
		init();

		this.onopen = function(){};
		this.oninit = function(){};
		this.onmessage = function(){};
		this.ondisconnect = function(){};
		this.onclose = function(){};
		this.onheartbeat = function(){};

		this.setURL = function(newURL){
			url = newURL;
		};
		this.send = function(data){
			if (transport){
				return transport.send(data);
			} else{
				return false;
			}
		};
		this.close = function(){
			readyState = CLOSING;
			if (transport){
				transport.close();
			}
		};
	};

	return stream;
}})})(jQuery);
