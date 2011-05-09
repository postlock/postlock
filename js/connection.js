(function() {
if (POSTLOCK) POSTLOCK.internal.set("modules.connection", function(spec) {
    var instance = this,
        invoke = POSTLOCK.internal.make_invoke_fun(instance),
       	my = {
        cb: spec.cb || 
            invoke("modules.callback_manager", {name:"gateway connection"}),
        url: spec.url,
        connection: null,
        state: 'idle',
        message_id_counter: invoke("modules.counter"),
        fun: {
                disconnect: function() {
                    if (my.state !== 'idle') {
                        my.fun.connection_state_change("disconnect", 'idle');
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
                        // add 'id' field to message
                        msg['id'] = my.fun.get_message_id(); 
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
                connection_state_change: function(message, new_state) {
                    var oldstate = my.state;
                    if (oldstate === new_state) return;
                    my.state = new_state;
                    // update current handle_incoming_msg function
                    my.fun.handle_incoming_msg.current = my.fun.handle_incoming_msg[my.state];
                    my.cb.fire_async("connection_state_change", [oldstate, new_state, message]);
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
                        var realm = msg_obj.body.realm || 
                                invoke('util.throw_ex', ["no realm provided", {msg_body: msg_obj.body}]),
                            uri = msg_obj.body.uri || 
                                invoke('util.throw_ex', ["no uri provided", {msg_body: msg_obj.body}]),
                            nonce = msg_obj.body.nonce || 
                                invoke('util.throw_ex', ["no nonce provided", {msg_body: msg_obj.body}]),
                            opaque = msg_obj.body.opaque || 
                                invoke('util.throw_ex', ["no opaque provided", {msg_body: msg_obj.body}]),
                            algorithm = msg_obj.body.algorithm || 
                                invoke('util.throw_ex', ["no algorithm provided", {msg_body: msg_obj.body}]),
                            alg_impl = POSTLOCK.internal.get('util.crypto.'+algorithm),
                            qop, HA1, HA2, nc, cnonce, response;

                        if (typeof(alg_impl) !== "function") {
                            invoke('util.throw_ex', ["algorithm not implemented", {algorithm: algorithmj}]);
                        }
                        qop = msg_obj.body.qop || 
                            invoke('util.throw_ex', ["no qop provided", {msg_body: msg_obj.body}]);
                        if (qop != "auth") {
                            invoke('util.throw_ex', ["qop not supported", {qop: qop}]);
                        }
                        HA1 = alg_impl([spec.username, realm, spec.password].join(":"));
                        HA2 = alg_impl(["POST", uri].join(":"));
                        nc = "00000001";
                        cnonce =  Math.random().toString();
                        response = alg_impl([HA1, nonce, nc, cnonce, qop, HA2].join(":"));
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
                                if (!("challenge_type" in msg_obj.body)) {
                                    invoke('util.throw_ex', ["bad auth challenge", {challenge: msg_obj}]);
                                }
                                if (!(msg_obj.body["challenge_type"] in my.fun.auth)) {
                                    invoke('util.throw_ex', [
                                        "unsupported auth type: "+ msg_obj.body["challenge_type"],
                                        {challenge: msg_obj}
                                    ]);
                                }
                                my.fun.websocket_safe_send({
                                    id: my.fun.get_message_id(), 
                                    type: "auth_response", 
                                    body: my.fun.auth[msg_obj.body["challenge_type"]](msg_obj)
                                });
                                break;
                            case "auth_success":
                                // set client id.
                                instance.config['participant_id'] = msg_obj.body.participant_id;
                                instance.config['session_id'] = msg_obj.body.participant_id;
                                my.fun.connection_state_change(msg_obj, 'connected');
                                my.cb.fire_async('connected');
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
        // create utility function for getting next message id
        my.fun.get_message_id = function() {return my.message_id_counter.get_value();};
        my.fun.handle_incoming_msg.current = my.fun.handle_incoming_msg.idle;
        // Register callbacks
        // handle incoming message from server
        my.cb.set_internal_cb("ws_onopen", function () {
            my.fun.websocket_safe_send({id: my.fun.get_message_id(), type: "client_connect"});
            // We don't want users hooking into ws_onopen.
            // Use the 'connection_state_change' event instead.
            return {skip_user_cb: true};
        });
        my.cb.set_internal_cb("ws_onmessage", function (raw_msg) {
            var msg;
            // Try to process the JSON message
            try {
                msg = JSON.parse(raw_msg.data);
                if (msg.type === 'error') {
                    // If the message is an error, handle it accordingly
                    my.cb.fire_async('error', ['remote_error', msg]);
                } else {
                    if (my.fun.handle_incoming_msg.current(msg) !== true) {
                        my.cb.fire_async("error", ['unexpected_message', my.state, msg]);
                    }
                }
            } catch (ex) {
                my.cb.fire_async('error', ['exception', ex, raw_msg]);
            }
        });
        my.cb.set_internal_cb("ws_onclose", function () {
            // forward this to the signal handler for 'error'.
            my.cb.fire_async('connection_closed');
        });
        my.cb.set_internal_cb("ws_error", function () {
            // forward this to the signal handler for 'error'.
            my.cb.fire_async('error', ['websocket', ex, invoke("util.args2array",arguments)]);
        });
        my.cb.set_internal_cb("error", function() {
            // On a remote error, the server will close the
            // websocket, we we don't have to.
            if (arguments[0] !== 'remote_error') my.fun.disconnect();
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
