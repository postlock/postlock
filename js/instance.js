/*
 * file: instance.js
 * author(s): Peter Neumark <neumark@postlock.org>
 * ------------------------------------------------------------------
 * Creates a postlock instance (of which there can be several).
 * ------------------------------------------------------------------
 */
(function() {
if (POSTLOCK) POSTLOCK.set("modules.instance", function(spec) {
    // POSTLOCK instance is stored in this variable.
    // all non-static code and objects are children.
    var instance = {
            exports: {}, 
            config: {},
            data: {}
        },
        fun = {},
        invoke = function(fun, args) {
            var a = args || [], real_args = ('length' in a)?a:[a];
            return POSTLOCK.get(fun).apply(instance, real_args);
        };

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
        any_field: '__any__',
        fields: ['from','type'],
        fallback_destination:  instance.cb.wrap_signal("no_destination_for_message")
    });
    instance.state_storage = invoke("modules.state_storage");
    // ---- message routing configuration ----
    // All incoming messages are sent to the message router.
    instance.cb.set_internal_cb('participant_message', 
        instance.message_router.handle_incoming);
    // Define a route for transactions (participant id 1 hardcoded for now)
    instance.message_router.add_route({from: 1, type: 'transaction'},
        instance.state_storage.receive_transaction);
    instance.message_router.add_route({from: 1, type: 'transaction_error'},
        instance.state_storage.receive_transaction_error);



    // ---- data belonging to postlock instance ----
    //instance.data.counters.client_message_id = invoke("modules.counter");
    //instance.data.counters.last_server_msg_id = 0;
    //instance.data.objects.set(invoke("modules.meta_object", ({cb: instance.cb})));

    // ---- utility functions ----
    fun.get_oid = function() {return instance.data.counters.object_id.gensym();};

    // ---- exports for instance (available to API user) ----
    // connection handling:
    instance.exports.connect = function() {instance.connection.connect(); return instance.exports;};
    instance.exports.disconnect = function() {instance.connection.disconnect(); return instance.exports;};
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
    instance.exports.set_cb = function() {instance.cb.set_user_cb.apply(this, arguments); return instance.exports;};
    // create functions for postlock objects:
    instance.exports.make_data = function (initial_value, transaction) {
        return invoke("modules.data", [{
            oid: fun.get_oid(),
            type: 'data',
            value: initial_value
        }, transaction]);
    };
    instance.exports.make_dict = function (transaction) {
        return invoke('modules.dict', [{
            oid: fun.get_oid(),
            type: 'dict'
        }, transaction]);
    };
    instance.exports.make_transaction = function () {
        return invoke("modules.transaction");
    };

    // CREATE meta-object
    return instance.exports;
}); 
})();
