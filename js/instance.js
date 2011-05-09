/*
 * file: instance.js
 * author(s): Peter Neumark <neumark@postlock.org>
 * ------------------------------------------------------------------
 * Creates a postlock instance (of which there can be several).
 * ------------------------------------------------------------------
 */
(function() {
if (POSTLOCK) POSTLOCK.internal.set("modules.instance", function(spec) {
    // POSTLOCK instance is stored in this variable.
    // all non-static code and objects are children.
    var instance = {
            exports: {}, 
            config: {},
            data: {}
        },
        invoke = POSTLOCK.internal.make_invoke_fun(instance),
        make_spec = function(type) {
            return {
                oid: instance.oid_counter.get_value(),
                type: type,
                postlock_instance: instance,
            };
        },
        i;
    // ---- module objects belonging to this postlock instance ----
    instance.cb = invoke("modules.callback_manager", {
        name:"main postlock object"
    });
    instance.connection = invoke("modules.connection", {
        cb: instance.cb, 
        url: spec.url,
        password: spec.password,
        username: spec.username
    });
    instance.message_router = invoke("modules.message_router", {
        fields: ['from','type'],
        fallback_destination:  instance.cb.wrap_signal("no_destination_for_message")
    });
    instance.state = invoke("modules.state");
    instance.oid_counter = invoke("modules.counter", [1]);
    // ---- message routing configuration ----
    // All incoming messages are sent to the message router.
    instance.cb.set_internal_cb('participant_message', 
        instance.message_router.handle_incoming);
    // Define a route for transactions (participant id 1 hardcoded for now)
    instance.message_router.add_route({from: 1, type: 'transaction'},
        instance.state.receive_transaction);
    instance.message_router.add_route({from: 1, type: 'transaction_error'},
        instance.state.receive_transaction_error);

    // ---- exports for instance (available to API user) ----
    // connection handling:
    instance.exports.connect = function() {
        instance.connection.connect(); 
        return instance.exports;
    };
    instance.exports.disconnect = function() {
        instance.connection.disconnect(); 
        return instance.exports;
    };
    instance.exports.is_connected = instance.connection.is_connected;
    // session information:
    instance.exports.session_id = function() {return instance.config.session_id+0;};
    instance.exports.participant_id = function() {return instance.config.participant_id+0};
    // messaging: 
    instance.exports.send = function(message) {instance.connection.send(message); return instance.exports;};
    instance.exports.add_route = function(selector, dest) {
        instance.message_router.add_route(selector, dest); 
        return instance.exports;
    };
    instance.exports.remove_route = function(selector, dest) {
        instance.message_router.remove_route(selector, dest); 
        return instance.exports;
    };
    // callback registration: 
    instance.exports.set_cb = function() {
        instance.cb.set_user_cb.apply(this, arguments);
        return instance.exports;
    };
    // exports from the state module
    invoke('util.shallow_copy', [instance.state.exports, instance.exports]);
    // create functions for postlock objects:
    instance.exports.make_data = function (initial_value) {
        var spec = make_spec('data');
        spec.state = initial_value;
        return invoke("modules.datatypes.make_with_create_op", spec).exports;
    };
    instance.exports.make_dict = function () {
        return invoke("modules.datatypes.make_with_create_op", 
            make_spec('dict')).exports;
    };
    instance.exports.make_list = function () {
        return invoke("modules.datatypes.make_with_create_op", 
            make_spec('list')).exports;
    };
    return instance.exports;
}); 
})();
