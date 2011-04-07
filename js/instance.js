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

    // --- module objects belonging to this postlock instance ----
    instance.cb = invoke("modules.callback_manager", {
        name:"main postlock object",
    });
    instance.connection = invoke("modules.connection", {
        cb: instance.cb, 
        url: spec.url,
        password: spec.password,
        username: spec.username
    });

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
