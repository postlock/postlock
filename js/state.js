/*
 * file: state.js
 * author(s): Peter Neumark <neumark@postlock.org>
 * ----------------------------------------------------------------------------
 * Handles storage of postlock shared state. Shared state is composed of
 * postlock objects. Each object is represented by a single object from the
 * datatypes module, and a reference to each is stored in my.objects.
 * Each transaction may create/delete/modify any object. In order to isolate
 * transactions from each other (allowing rollback of failed transactions), a
 * queue of transactions is maintained, with a Context object attached to each
 * transaction. The Context objects are connected in such a way that if 
 * an object was not touched by the last transaction (and therefore not present
 * in its context), the previous context will be search and so on.
 * 
 * The trick is that the objects themselves are stored in my.objects, but the
 * interal state of the object is stored in the context belonging to the
 * transaction. This means that a reference to the object remains valid even
 * when the context changes due to a new transaction.
 *
 *              local transaction 2
 *              --------------------
 *              local transaction 1    Queued
 *              remote transaction 0   Queued
 *              local transaction 0    Pending acknowledgement
 *              --------------------
 *              acknowledged transaction(s)
 *
 * The local transaction currently under composition (if on exists) is at the
 * top of the queue (local transaction 2 above). It's context can be altered.
 * Below it is the queue of transactions, each of which has been closed,
 * waiting to be sent to the server. The transaction at head of the queue
 * has already been sent to the server, and is waiting for the (N)ACK. If the
 * server ACK's the transaction, it's context is merged into the acknowledged
 * context and the transaction is discarded. If a NACK arrives, then the it is
 * rolled back.
 * Remote transactions are placed in the transaction queue just like local
 * transactions. When a remote transaction reaches the bottom of the queue, it
 * is automatically merged into the acknowledged context.
 *
 * ----------------------------------------------------------------------------
 */

(function() {
if (POSTLOCK) POSTLOCK.set("modules.state", function(spec) {
    var instance = this,
        invoke = function(fun, args) {
            var a = args || [], real_args = ('length' in a)?a:[a];
            return POSTLOCK.get(fun).apply(instance, real_args);
	    },
        my = {
            // Transactions will most likely fail on the server,
            // but local failure cannot be ruled out, which will
            // result in the following nice error messages passed
            // to the api user in an exception. User parseInt to
            // get the numeric error id from the string.
            error_msg: {
                'INVALID_OID':
                    '100 Invalid OID',
                'INVALID_CMD':
                    '101 Invalid operation for object',
                'INVALID_PARAMS':
                    '102 Invalid parameters provided for operation'
            },
            // Stores postlock objects as 'oid' -> object.
            objects: {},
            // The queue of transactions (see comment at top of file for
            // a brief explanation).
            transaction_queue: [], 
            // A dummy transaction, important only for its context which
            // contains the combined state of all ack'd transactions so far.
            acknowledged_transaction: null,
            // server_state_version is the version number of the last
            // successfully executed server transaction
            server_state_version: 0, // TODO: intialize to real value!
            fun: {}
        };
    my.transaction_queue.waiting_for_server = false;
    my.transaction_queue.flush = function() {
        // If we can send the transaction at the head of the
        // queue to the server, then do it.
        if (my.transaction_queue.length === 0 ||
            my.transaction_queue[0].open ||
            my.transaction_queue.waiting_for_server) {
            return false;
        }
        my.transaction_queue.waiting_for_server = true;
        return my.transaction_queue[0].send();
    };
    my.fun.current_transaction = function () {
        if (my.transaction_queue.length > 0) {
            return my.transaction_queue[
                my.transaction_queue.length -1];
        }
        return my.acknowledged_transaction;
    };
    my.fun.get_object = function(oid) {
        return my.objects[oid];
    };
    // run_in_transaction guarantees that the current transaction
    // will be open by creating a new transaction if necessary.
    // Transactions created in this function will be closed
    // automatically.
    // Passing a msg parameter makes the transaction remote.
    my.fun.run_in_transaction = function(fun, msg) {
        var result,
            transaction = my.fun.current_transaction(),
            create_transaction = !transaction.open,
            exception = null;
        if (create_transaction) {
            // note: by default new transactions are local
            transaction = new my.fun.Transaction(msg); 
            // add this object to the transaction queue
            my.transaction_queue.push(transaction);
        }
        try {
            result = fun();
        } catch (e) {
            exception = e;
        }
        if (create_transaction) {
            // note: by default new transactions are local
            transaction.close();
        }
        if (exception !== null) {
            throw exception;
        }
        return result;
    };
    // Creates a "participantid.messageid" format
    // string used in 'in_response_to' field of the server's ACKs.
    my.fun.in_response_to_msg_id = function(msg_id) {
        return instance.exports.participant_id + "." + msg_id;
    };
    // regular javascript constructor (to be used with new)
    // to create a context object.
    my.fun.Context = function (spec) {
        spec = spec || {};
        if (spec.backing_context) {
            this.backing_context = spec.backing_context;
        }
        this.transaction = spec.transaction;
        // options that can be passed in 'spec':
        // backing_context: ctxt
        //      Useful if this context is used to
        //      run a transaction. In such a case
        //      a get(oid) should return the
        //      backing store's instance of the
        //      object it it does not have such an
        //      object itself.
        //      The backing context is never modified!
        // ---- initialize data first ----
        // object_state contains the oid -> object state
        // mapping for objects that were touched in the context
        this.object_state = {};
        // during transactions, we want the
        // list of deleted objects instead
        // of actually removing dictionary 
        // elements. To actually commit
        // the delete, call flush_deleted().
        this.deleted = {};
    }; // end Context constructor
    // ---- Context functions ----
    my.fun.Context.prototype.get_ref = function (oid) {
        if (this.deleted.hasOwnProperty(oid)) {
            return undefined;
        }
        if (this.object_state.hasOwnProperty(oid)) {
            return this.object_state[oid].clone();
        }
        if (this.backing_context && 
            this.backing_context.has(oid)) {
            return this.backing_context.get(oid);
        }
        // No object with the given oid was found in
        // this context or the optional backing context.
        return undefined;
    };
    my.fun.Context.prototype.get = function (oid) {
    // get(oid) returns the internal state of object oid
    // in the current context. If the current transaction's
    // own context has an instance of oid, then the reference
    // is returned. Otherwise, a clone of the state is returned.

        var o = this.get_ref(oid);
        if (typeof(o) === 'object') {
            if (!this.object_state.hasOwnProperty(oid)) {
                o = o.clone();
            }
        }
        return o;
    };
    my.fun.Context.prototype.set = function (oid, state) {
        // if the object was previously on the 
        // 'deleted' list, remove it from the list.
        if (this.deleted.hasOwnProperty(oid)) {
            delete this.deleted[oid];
        }
        this.object_state[oid] = state;
    };
    my.fun.Context.prototype.del = function (oid) {
        // if oid is already deleted, do nothing
        if (this.deleted.hasOwnProperty(oid)) {
            return false;
        }
        // otherwise add to list of deleted objects
        this.deleted[oid] = true;
    };
    my.fun.Context.prototype.has = function (oid) {
        if (this.object_state.hasOwnProperty(oid) &&
            !this.deleted.hasOwnProperty(oid)) {
            return true;
        }
        if (this.backing_context) {
            return this.backing_context.has(oid);
        }
        return false;
    };
    my.fun.Context.prototype.flush_deleted = function () {
        // remove deleted objects from this.object_state.
        // not all objects in this.deleted can be
        // flushed, since some of them could be in
        // the backing context, which is not modified.
        var i, flushed = 0;
        for (i in this.deleted) {
            if (this.deleted.hasOwnProperty(i) &&
                this.object_state.hasOwnProperty(i)) {
                flushed += 1;
                delete this.object_state[i];
                delete this.deleted[i];
            }
        }
        return flushed;
    };
    my.fun.Context.prototype.merge = function (newer_context) {
        // merge the changes from a newer context into this context
        // used to push ACKd transactions into the acknowledged context.
        // the object in newer_context.object_state replace this.object_state.
        // Objects in newer_context.deleted should be deleted.
        var i;
        for (i in newer_context.object_state) {
            if (newer_context.object_state.hasOwnProperty(i)) {
                this.object_state[i] = newer_context.object_state[i];
            }
        }
        // Since we'll be doing the merge on the acknowledged context,
        // this.deleted will be {}, so we lose nothing.
        this.deleted = newer_context.deleted;
        this.flush_deleted();
    };
    // regular JS constructor (for use with new) for
    // transaction objects.
    // msg: msg containing transaction in the remote case
    my.fun.Transaction = function (msg) {
        if (msg) {
            // remote transaction
            this.msg = msg;
            this.ops = msg.body.ops;
            this.local = false;
        } else {
            // local transaction
            this.open = true;
            this.local = true;
            this.ops = [];
        }
        this.context = new my.fun.Context({
            backing_context: 
                my.fun.current_transaction().context,
            transaction: this
        });
    };
    // ---- methods ----
    my.fun.Transaction.prototype.add_op = function(op) {
        this.ops.push(op);
    };
    my.fun.Transaction.prototype.close = function() {
        this.open = false;
        // Record server state version, needed by OT.
        this.server_state_version = my.server_state_version;
        // If this is the only waiting transaction
        // then send it to the server right away.
        if (transaction_queue.length > 0) {
            transaction_queue.flush();
        }
    };
    my.fun.Transaction.prototype.send = function() {
        this.msg = {
            state_version: my.server_state_version,
            ops: this.ops
        };
        instance.connection.send(this.msg);
    };
    // ---- processing incoming messages ----
    my.fun.ack_transaction = function() {
        my.transaction_queue.waiting_for_server = false;
        // merge the acknowledged transaction into the
        // acknowledged context
        my.acknowledged_transaction.context.merge(
            my.transaction_queue[0].context);
        // get rid of the transaction
        my.transaction_queue.shift();
        // as merge and remote transactions at the head of
        // the queue.
        my.transaction_queue.flush();
    };
    my.fun.receive_transaction = function (msg) {
        // The incoming message is either a broadcast of someone
        // else's transaction, or an ACK of our transaction
        if (msg.body.hasOwnProperty('in_response_to') &&
            my.transaction_queue.length > 0 &&
            my.transaction_queue[0].open === false &&
            my.fun.in_response_to_msg_id(my.transaction_queue[0].msg.id) === 
                msg.body.in_respone_to) {
            my.fun.ack_transaction();
        } else {
            my.fun.run_remote_transaction(msg);
        }
        // update my.server_state_version. We may have received
        // remote transactions that are newer than our ack,
        // so we take the maximum.
        my.server_state_version = Math.max(
            my.server_state_version,
            msg.id);
    };
    my.fun.receive_transaction_error = function (msg) {
    };
    my.fun.run_remote_transaction = function(msg) {
        var i, fun, obj,
            make_err: function(reason, e) {
                var err = {
                    success: false,
                    reason: reason,
                    failed_op: i
                };
                if (e) {
                    err.exception = e;
                }
                return err;
            };
        my.fun.run_in_transaction(function () {
            for (i = 0; i < msg.body.ops.length; i++) {
                if (msg.body.ops[i].hasOwnProperty('oid')) {
                    // if the op has a 'oid' field, pass it to the
                    // associated object
                    obj = my.fun.get_object(msg.body.ops[i].oid);
                    if (obj === undefined) {
                        return make_err(my.error_msg.INVALID_OID);
                    }
                    fun = obj[msg.body.ops[i].cmd];
                    if (typeof(fun) !== 'function') {
                        return make_err(my.error_msg.INVALID_CMD);
                    }
                    try {
                        obj[fun].apply(obj, msg.body.ops[i].params) 
                    } catch (e) {
                        return make_err(my.error_msg.INVALID_PARAMS, e);
                    }
                } else {
                    // the operation is a create or delete
                    switch (msg.body.ops[i].cmd) {
                        case 'create':
                            break;
                        case 'delete':
                            break;
                        default:
                            invoke(
                                'util.throw_ex', 
                                ['bad cmd in op', msg.body.ops[i]]);
                    }
                }
            }
            return {success: true};
        }, msg);
    };
    // ---- initialize internal data structures ----
    my.acknowledged_transaction = new my.fun.Transaction();
    my.acknowledged_transaction.close();
    return {
        // ---- to be used internally by the postlock instance ----
        receive_transaction: my.fun.receive_transaction,
        receive_transaction_error: my.fun.receive_transaction_error,
        run_in_transaction: my.fun.run_in_transaction,
        current_transaction: my.fun.current_transaction
        // ---- to be exported to the api user ----
        exports: {
            run_in_transaction: my.fun.run_in_transaction,
            get_object: my.fun.get_objeet
        }
    };
}); 
})();
 
