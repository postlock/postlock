(function() {
if (POSTLOCK) POSTLOCK.set("modules.connection", function(spec) {
    var instance = this,
        invoke = function(fun, args) {
            var a = args || [], real_args = ('length' in a)?a:[a];
            return POSTLOCK.get(fun).apply(instance, real_args);
	},
   	my = {
        cb: spec.cb || 
            invoke("modules.callback_manager", {name:"gateway connection"}),
        url: spec.url,
        connection: null,
        state: 'idle',
        state_data: {},
        fun: {
                disconnect: function(reason) {
                    if (my.state !== 'idle') {
                        my.fun.connection_state_change("disconnect", 'idle', {reason: reason});
                        my.connection.close();
                    }
                },
                send: function(msg) {
                    if (my.state !== "connected") {
                        invoke("util.throw_ex", [
                            "Connection not yet ready!", {
                            state: my.state,
                            msg: msg }]);
                    } else {
                        return my.fun.websocket_safe_send(msg);
                    }
                },
                connect: function () {
                    my.connection=new WebSocket(my.url);
                    // Set callbacks for websocket events.
                    my.connection.onopen=my.cb.wrap_signal("ws_onopen");
                    my.connection.onmessage=my.cb.wrap_signal("ws_onmessage");
                    my.connection.onclose=my.cb.wrap_signal("ws_onclose");
                    my.connection.onerror=my.cb.wrap_signal("ws_onerror");
                },
                connection_state_change: function(message, new_state, state_data) {
                    var oldstate = my.state;
                    my.state = new_state;
                    my.state_data = state_data || {};
                    // update current handle_incoming_msg function
                    my.fun.handle_incoming_msg.current = my.fun.handle_incoming_msg[my.state];
                    my.cb.fire("connection_state_change", [oldstate, new_state, message]);
                },
                websocket_safe_send: function (data_obj) {
                    var data = JSON.stringify(data_obj);
                    invoke("util.retry_until", [
                        // condition
                        function () { return my.connection.readyState == 1;},
                        // on success
                        function () {
                            return my.connection.send(data);
                        }
                    ]);
                },
                auth: {
                    trivial: function() {
                        return {username: spec.username, password: spec.password};
                    },
                    digest: function(msg_obj) {
                        throw_ex = POSTLOCK.get("util.throw_ex");
                        var realm = msg_obj.body.realm || throw_ex("no realm provided", {msg_body: msg_obj.bodyj});
                        var uri = msg_obj.body.uri || throw_ex("no uri provided", {msg_body: msg_obj.bodyj});
                        var nonce = msg_obj.body.nonce || throw_ex("no nonce provided", {msg_body: msg_obj.bodyj});
                        var opaque = msg_obj.body.opaque || throw_ex("no opaque provided", {msg_body: msg_obj.bodyj});
                        var algorithm = msg_obj.body.algorithm || throw_ex("no algorithm provided", {msg_body: msg_obj.bodyj});
                        if (typeof(window[algorithm]) === "function") {
                            // TODO: get rid of window, don't use the global namesapce.
                            algorithm = window[algorithm];
                        } else {
                            throw_ex("algorithm not implemented", {algorithm: algorithmj});
                        }
                        var qop = msg_obj.body.qop || throw_ex("no qop provided", {msg_body: msg_obj.bodyj});
                        if (qop != "auth") {
                            throw_ex("qop not supported", {qop: qop});
                        }

                        var HA1 = algorithm([spec.username, realm, spec.password].join(":"));
                        var HA2 = algorithm(["POST", uri].join(":"));
                        var nc = "00000001";
                        var cnonce =  Math.random().toString();
                        var response = algorithm([HA1, nonce, nc, cnonce, qop, HA2].join(":"));

                        return {
                            username: spec.username,
                            nc: nc,
                            cnonce: cnonce,
                            response: response,
                            opaque: opaque
                        };
                    }
                },
                // handle_incoming_msg.current is overwritten when a state change requires incoming messages
                // to be handled in a new way. The handler for each state is under
                // handle_incoming_msg[state].
                handle_incoming_msg: {
                    current: null,
                    idle:  function (msg_obj) {
                        if (msg_obj.type === "server_connect") {
                            my.fun.connection_state_change(msg_obj, 'auth');
                            return true;
                        }
                    },
                    auth: function (msg_obj) {
                        switch (msg_obj.type) {
                            case "auth_challenge": 
                                if (!("challenge_type" in msg_obj.body)) throw_ex("bad auth challenge", {challenge: msg_obj});
                                if (!(msg_obj.body["challenge_type"] in my.fun.auth)) throw_ex("unsupported auth type: "+ msg_obj.body["challenge_type"], {challenge: msg_obj});
                                my.fun.websocket_safe_send({type: "auth_response", body: my.fun.auth[msg_obj.body["challenge_type"]](msg_obj)});
                                break;
                            case "auth_result":
                                switch ((msg_obj.body && ("result" in msg_obj.body))?msg_obj.body['result']:"") {
                                    case 'success':
                                        // set client id.
                                        instance.data.config['participant_id'] = msg_obj.body.participant_id;
                                        instance.data.config['session_id'] = msg_obj.body.participant_id;
                                        // set prefix for oid's created by this participant.
                                        instance.data.counters.object_id.set_prefix(msg_obj.body.participant_id + ".");
                                        my.fun.connection_state_change(msg_obj, 'connected');
                                        my.cb.fire_async('connected');
                                        break;
                                    case 'failure':
                                        my.fun.disconnect("auth_failure");
                                        break;
                                    default:
                                        // missing or unexpected "result" -> bad message!
                                        return false;
                                };
                                break;
                            default:
                               return false;
                        };
                        return true;
                    },
                    connected: function (msg_obj) {
                        // In the connected state, we will decide what to do with the object based on the 
                        // 'type' field. The default is to pass the message to the user callback.
                        if (!('type' in msg_obj)) return false;
                        switch (msg_obj['type']) {
                            case 'transaction':
                                // STUB
                                break;
                            case 'acked_message':
                                // STUB
                                break;
                            default:
                                my.cb.fire_async('participant_message', [msg_obj]);
                        };
                        return true;
                    }
                }   // end handle_incoming_msg
            }       // end fun
        };          // end my
        my.fun.handle_incoming_msg.current = my.fun.handle_incoming_msg.idle;
        // Register callbacks
        // handle incoming message from server
        my.cb.set_internal_cb("ws_onopen", function () {
            my.fun.websocket_safe_send({type: "client_connect"});
            // We don't want users hooking into ws_onopen.
            // Use the 'connection_state_change' event instead.
            return {skip_user_cb: true};
        });
        my.cb.set_internal_cb("ws_onmessage", function (msg) {
            var ret, t = null;
            try {
                t = JSON.parse(msg.data);
            } catch (e) {
                my.cb.fire("error", ["parsing", e, msg]);
            }
            if (t !== null)  {
                try {
                    ret = my.fun.handle_incoming_msg.current(t);
                } catch (e) {
                    // TODO: maybe fire a callback...
                    my.cb.fire("error",["message_processing", e]);
                }
                if (!ret) {
                    if (t.type === "error") my.cb.fire("error", ["remote_error", t]);
                    else my.cb.fire("error", ["unexpected_message", my.state, t]);
                }
            }
            else console.log("failed to parse incoming message '" + msg.data +"'");
        });
        my.cb.set_internal_cb("ws_onclose", function () {
            // TODO: attempt to re-open the connection
            var a = invoke("util.args2array", arguments);
            console.log("ws_onclose fired, arguments: " + a);
        });
        my.cb.set_internal_cb("ws_error", function () {
            // TODO: attempt to handle the error
            console.log("ws_error fired, arguments: " + invoke("util.args2array",arguments));
        });
        // return exports
        return {
            connect: my.fun.connect,
            send: my.fun.send,
            disconnect: my.fun.disconnect,
            is_connected: function() {my.state === "connected";},
            cb: my.cb // note: shouldn't be passed to the user
        };
})})();
