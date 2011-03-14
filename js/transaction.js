(function() {
if (POSTLOCK) POSTLOCK.set("modules.transaction", function (s) {
    var spec = s || {},
        my = {
            msg: spec.msg || {
                type: 'transaction',
                header: {},
                ts: {
                    created: POSTLOCK.get("util.get_timestamp")()
                },
                body: {
                    transformations: []
                }
        }, 
        // transaction states:
        // init - transaction is under composition (initial state)
        // applied - (local only) the transaction has been locally applied, ACK from server pending
        // acknowledged - the transaction has been acknowledged (for remote transactions this is the same as applied)
        // aborted - the transaction has been rolled back
        current_status: "init",
        is_remote: spec.is_remote || false,
        fun: {}
    };

    my.fun.get_tid = function () {
        if (my.msg.header.id !== undefined) return ((my.is_remote)?'s':'c') + my.msg.header.id;
        return "[none]";
    };

    my.fun.make_t_map = function () {
        // rearrange transformation list into an
        // object id -> list of commands map.
        var i, oid, t_map = {};
        for (i = 0; i < my.msg.body.transformations.length; i++) {
            oid = my.msg.body.transformations[i].oid;
            if (!(oid in t_map)) t_map[oid] = [];
            t_map[oid].push(my.msg.body.transformations[i]);
        }
        return t_map;
    };

    my.fun.rollback = function () {
        var i, t_map = arguments[0] || my.fun.make_t_map(),
            e = arguments[1] || {message: "no reason given", data: {}};
        console.log("starting rollback of transaction "+my.fun.get_tid()+"; reason: "+e.message);
        for (i in t_map) {
            if (!t_map.hasOwnProperty(i)) continue;
            POSTLOCK.get("util.fire")(i, 'rollback_transaction',[my.fun.get_tid()]);
        }
        my.current_status = "aborted";
        // unregister transaction
        POSTLOCK.get("transactions.get")(my.fun.get_tid());
    };

    my.fun.apply_locally = function () {
        var i, t_map = my.fun.make_t_map();
        try {
            for (i in t_map) {
                if (!t_map.hasOwnProperty(i)) continue;
                // 'apply_transaction' handler will throw ex if transaction cannot be applied.
                POSTLOCK.get("util.fire")(i, "apply_transaction", [my.fun.get_tid(), my.is_remote, t_map[i]]);
            }
            my.current_status = "applied";
            return true;
        } catch (e) {
            my.fun.rollback(t_map, e);
        }
        return true;
    };
    my.fun.send = function () {
        POSTLOCK.get("outqueue.add_transaction")(my.msg);
    };
    my.fun.apply = function () {
        // add id to local transactions
        if (my.msg.header.id === undefined) POSTLOCK.get("util.add_message_id")(my.msg.header);
        POSTLOCK.get("transactions.save")(my.ret);
        if (my.fun.apply_locally()) {
            if (my.is_remote) my.current_status = "acknowledged";
            else my.fun.send();
            return true;
        }
        return false;
    };
    my.fun.ack = function () { // only called for local transactions
        var t_map = my.fun.make_t_map();
        for (i in t_map) {
            if (!t_map.hasOwnProperty(i)) continue;
            POSTLOCK.get("util.fire")(i, "ack_transaction", [my.fun.get_tid()]);
        }
        my.current_status = "acknowledged";
        // unregister transaction
        POSTLOCK.get("transactions.remove")(my.fun.get_tid());
    };
    // RETURN EXPORTS
    my.ret = {
        set_current_state: function (new_status) {my.current_status = new_status;},
        get_transformations: function () {return my.msg.body.transformations;},
        ack: my.fun.ack,
        rollback: my.fun.rollback,
        exports: {
            is_remote: function () {return my.is_remote === true;},
            add_transformation: function (t) {
                if (my.current_status !== "init") {
                    POSTLOCK.get("util.throw_ex")("add_transformation failed: transaction not in init state", t);
                }
                my.msg.body.transformations.push(t);
                return t;
            },
            get_tid: my.fun.get_tid,
            get_current_status: function () {return my.current_status+"";},
            apply: my.fun.apply
        }
    };
    // register this transaction
    return my.ret.exports;
};
)})();
