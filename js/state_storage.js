(function() {
if (POSTLOCK) POSTLOCK.set("modules.state_storage", function(spec) {
    // POSTLOCK state storage module's code.
    // This module is responsible for:
    // - storing/retrieving objects
    // - adding objects from another state_storage instance
    // - applying 'meta object operations' (create, delete).
    var instance = this,
        invoke = function(fun, args) {
            var a = args || [], real_args = ('length' in a)?a:[a];
            return POSTLOCK.get(fun).apply(instance, real_args);
	    },
        my = {
            error_msg: {
                'INVALID_OID':
                    '100 Invalid OID',
                'INVALID_CMD':
                    '101 Invalid operation for object',
                'INVALID_PARAMS':
                    '102 Invalid parameters provided for operation'
            },
            // queue for outgoing unacknowledged transactions
            queue: [], 
            unacknowledged: {},
            objects: {
                meta_object: {
                    op_create: function() {
                    },
                    op_delete: function() {
                    }
                }
            },
            fun: {
                // ---- functions for object storage/retrieval ----
                load: function (oid, context) {return my.objects[oid];},
                save: function (obj, context) {my.objects[oid] = obj;},
                // ---- processing incoming messages ----
                receive_transaction: function(msg) {
                    // The incoming message is either a broadcast of someone
                    // else's transaction, or an ACK of our transaction
                    if (msg.body.hasOwnProperty('in_response_to')) {
                        // we got an ACK: update queue
                        if (typeof(my.unacknolwedged) === 'object' &&
                            my.unacknwoledged.id === msg.body.in_response_to) {
                            // change the state of the unacknowedged transaction
                            my.unacknowledged.ack();
                            my.unacknowledged = queue.pop();
                            if (my.unacknowledged !== undefined) {
                                // send the next transaction waiting in the queue
                                instance.connection.send(my.unacknowledged);
                            }
                        } else {
                            // ACK received for different transaction than anticipated
                            console.log("Unexpected ACK");
                        }
                    } else {
                        // we got someone's transaction: run it
                        try {
                            my.fun.run_transaction(msg);
                        } catch (e) {
                            console.log("error executing transaction:");
                            console.log(e);
                        }
                    }
                },
                receive_transaction_error: function(msg) {
                },
                // ---- execution of transactions ----
                run_transaction: function(msg, ctxt) {
                    var context = ctxt || { 
                            touched: {},
                            deleted: {}
                        },
                        i,
                        make_err: function(reason) {
                            return {
                                context: context,
                                success: false,
                                reason: reason,
                                failed_op: i
                            };
                        },
                        fun,
                        obj;
                    for (i = 0; i < msg.body.ops.length; i++) {
                        if (!msg.body.ops[i].hasOwnProperty('oid')) {
                            msg.body.ops[i].oid = 'meta_object';
                        }
                        // if the op has a 'oid' field, pass it to the
                        // associated object
                        obj = my.fun.get(msg.body.ops[i].oid, context);
                        if (obj === undefined) {
                            return make_err(my.error_msg.INVALID_OID);
                        }
                        fun = 'op_' + obj[msg.body.ops[i].cmd];
                        if (typeof(fun) !== 'function') {
                            return make_err(my.error_msg.INVALID_CMD);
                        }
                        obj = obj[fun].apply(my.exports, msg.body.ops[i].params);
                        if (typeof(obj) !== 'object') {
                            return make_err(my.error_msg.INVALID_PARAMS);
                        }
                        // at this point, the transaction should be successful,
                        // so we save the new object to the context
                        my.fun.save(obj, context);
                        // we return the result of the operation:
                    } 
                    return {
                            success: true,
                            context: context,
                    };
                },
            } // end my.fun
        };
        my.exports = {
        get: my.fun.get,
        set: my.fun.set,
        get_message: my.fun.get_message,
    };
    return my.exports;
}); 
})();
 
