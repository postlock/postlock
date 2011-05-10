/* file: datatypes.js
 * author(s): Peter Neumark <neumark@postlock.org>
 * ----------------------------------------------------------------------------
 * The module defines postlock datatypes for use in js client api.
 * The objects have a prototypal inheritance structure:
 *          common_base
 *          -----------
 *               |
 *       dict   list   data
 *       ------------------
 *        |      |      |
 *      (object instances)
 *      ------------------
 * Since the internal data structures of a data type should not
 * be exposed to the api user, we cannot give users the object
 * instances directly. Instead, each instance has an exports object
 * which contains a set of functions that make up the api. The functions
 * in the exports object cannot use 'this', so they are bound to their
 * object instance. 
 *
 * These data type objects are used both by the API user when the
 * application needs to create a new postlock object and by state
 * when it executes a transaction broadcast by the state server. In the
 * latter case, object creation does not need to be wrapped in a transaction
 * and the exported functions are never used. Setting spec.internal to true
 * will make the objects work this way.
 *
 * The code in the datatypes module is independent of the postlock
 * instance, but the postlock object instances are tied to the instance
 * that creates them (which is passed in as spec.postlock_instance).
 *
 * In order to make the same object work in every context, the internal
 * state of objects is stored in the transaction's context, not in the
 * object itself.
 * A 'regular' javascript constructor, State() is required to exist for
 * each datatype. State is only required to have a single function: clone(),
 * which creates a copy of itself. The cloned version should be a deep copy
 * so that the two instances of the state object can be modified without
 * one affecting the other. 
 * 
 */
(function () {
    if (!POSTLOCK) return;
    POSTLOCK.internal.set("modules.datatypes", (function () {
            // invoke is not bound to a postlock instance
        var invoke = POSTLOCK.internal.make_invoke_fun(null),
            my = {
                constants: {
                    UNSAFE_PREFIX: "unsafe_",
                    DICT_PREFIX: "entry_"
                },
                fun: {
                    // functions to add metadata to other functions
                    api: function(fun) {
                        fun.type = 'api';
                        return fun;
                    },
                    op: function(fun) {
                        fun.type = 'op';
                        fun.has_unsafe = true;
                        return fun;
                    },
                    op_no_unsafe: function(fun) {
                        fun.type = 'op';
                        fun.has_unsafe = false;
                        return fun;
                    },
                    // helper functions for dicts
                    key2ix: function (key) {
                        return my.constants.DICT_PREFIX + key;
                    },
                    ix2key: function (ix) {
                        return ix.substr(my.constants.DICT_PREFIX.length);
                    }
                } // end my.fun
            }; // end my
            my.common_base = {
                // create exports object containing public functions
                // bound to the obj_instance
                make_exports: function () {
                    var i, 
                        exports = {}, 
                        obj_instance = this, 
                        add_op_to_transaction = function (is_safe, op, args) {
                            // 1. Performs the operation locally
                            // 2. Adds the operation to the current transaction.
                            // This order is important because if the operation failes
                            // (bad parameters, for example), then the transaction should
                            // not include the bad op.
                            try {
                                this.postlock_instance.state.run_in_transaction(function () {
                                    // apply the operation locally
                                    obj_instance[op].apply(obj_instance, args);
                                    // add the operation to the transaction
                                    obj_instance.postlock_instance.state.current_transaction().add_op({
                                        oid: obj_instance.oid(),
                                        cmd: op,
                                        safe: is_safe,
                                        params: op_parameters
                                    });
                                });
                            } catch (e) {
                                // The local operation could not be executed.
                                invoke('util.throw_ex', ["local operation failed", e]);
                            }
                        };
                    // iterate through each function in the entire
                    // prototype chain, exporting functions
                    // with the necessary metadata.
                    for (i in this) {
                        if (typeof(this[i]) === 'function' &&
                            this[i].type) {
                            switch (this[i].type) {
                                case 'api':
                                    // api functions are bound to the
                                    // object instance.
                                    exports[i] = (function () {
                                        var ix = i;
                                        return function() {
                                            return obj_instance[ix](arguments);
                                        }
                                    }());
                                    break;
                                case 'op':
                                    // op functions call invoke_op.
                                    // depending on the has_unsafe field,
                                    // there may be an 'unsafe_' version
                                    // exported as well.
                                    (function (o) {
                                        var ix = i;
                                        exports[ix] = function() {
                                            return add_op_to_transaction(true, ix, arguments);
                                        };
                                        if (o[ix].has_unsafe === true) {
                                            exports[my.constants.UNSAFE_PREFIX + ix] = function() {
                                                return add_op_to_transaction(false, ix, arguments);
                                            };
                                        }
                                    }(this));
                                    break;
                                default:
                                    // this shouldn't ever happen
                                    console.error('unexpected function type');
                            }
                        }
                    }
                    // Finally, add the callback manager's set_user_cb to the exports object.
                    exports.set_cb = obj_instance.cb.set_user_cb;
                    return exports;
                },     
                // Since context can change, there should be no references to state
                // shared by the methods of this object, get_state should be used
                // by each function!
                get_state: function() {
                    // get automatically clones state if it does not yet exist in the
                    // current context.
                    return this.postlock_instance.state.current_transaction().context.get(this.oid());
                },
                get_state_ref: function() {
                    return this.postlock_instance.state.current_transaction().context.get_ref(this.oid());
                },
                set_state: function(state) {
                    return this.postlock_instance.state.current_transaction().context.set(this.oid(), state);
                },
                get_object: function(oid) {
                    var o = this.postlock_instance.state.get_object(oid);
                    if (typeof(o) !== 'object') {
                        invoke('util.throw_ex', 'error getting object: '+oid);    
                    }
                    return o.exports;
                },
                init: function (spec) {
                    this.postlock_instance = spec.postlock_instance;
                    this.object_oid = spec.oid;
                    this.cb = this.postlock_instance.invoke("modules.callback_manager", {
                        name: "callback manager for "+spec.type+" with oid "+spec.oid
                    });
                    this.exports = this.make_exports();
                    this.set_state(new this.State(spec.state));
                    return this;
                },
                oid: my.fun.api(function() {
                    return this.object_oid;
                })
            };
            my.base_objects = {};
            /* The prototypes for the different data types follow.
             * Each prototype inherits from common_base, but is extended
             * with its own functions.
             */
            // ---- data object ----
            my.base_objects.data = invoke('util.shallow_copy', [{
                set: my.fun.op(function (value) {
                    this.set_state(new this.State(value));
                }),
                get: my.fun.api(function () {
                    // return a cloned version of internal state:
                    return this.get_state_ref().clone().data; 
                })
            }, Object.create(my.common_base)]);
            my.base_objects.data.State = function (data) {
                this.data = data;
            };
            my.base_objects.data.State.prototype.clone = function() {
                var cloned_data = invoke('util.clone', [this.data]);
                return new my.base_objects.data.State(cloned_data);
            };
            // ---- dict object ----
            my.base_objects.dict = invoke('util.shallow_copy', [{ 
                set: my.fun.op(function (key, object) {
                    var state = my.get_state();
                    state.data[my.fun.key2ix(key)] = object.oid();
                    this.set_state(state);
                }),
                unset: my.fun.op(function (key) {
                    var state = this.get_state();
                    var ix = my.fun.key2ix(key);
                    if (!state.data.hasOwnProperty(ix)) {
                        invoke("util.throw_ex", 'cannot unset missing key '+key);
                    }
                    delete state.data[ix];
                    this.set_state(state);
                }),
                get: my.fun.api(function (key) {
                    var state = this.get_state_ref();
                    var ix = my.fun.key2ix(key);
                    if (!state.data.hasOwnProperty(ix)) {
                        invoke("util.throw_ex", 'dict: key "'+key+'" is unbound');
                    }
                    // returns obj, not oid
                    return this.get_object(state.data[ix]);
                }),
                // note: the order of the keys is arbitrary.
                keys: my.fun.api(function () {
                    var state = this.get_state_ref();
                    var i, keys = [];
                    for (i in state.data) {
                        if (state.data.hasOwnProperty(i)) {
                            keys.push(my.fun.ix2key(i));
                        }
                    }
                    return keys;
                })
            }, Object.create(my.common_base)]);
            my.base_objects.dict.State = function (data) {
                this.data = data;
            };
            my.base_objects.dict.State.prototype.clone = function() {
                var cloned_data = invoke('util.shallow_copy', [this.data, {}]);
                return new my.base_objects.dict.State(cloned_data);
            };
            // ---- list object ----
            my.base_objects.list = invoke('util.shallow_copy', [{
                // inserts cannot lead to data loss, no
                // unsafe_insert necessary.
                insert: my.fun.op_no_unsafe(function (pos, object) {
                    var i, 
                        oid = object.oid(),
                        tmp_list,
                        state = this.get_state();
                    if (pos === state.data.length) {
                        // append to end of list
                        state.data.push(value);
                    }
                    else if (pos < state.data.length) {
                        // move later elements back
                        tmp_list = state.data.slice(0,pos);
                        tmp_list.push(oid);
                        for (i = pos; i < state.data.length; i++) {
                            tmp_list.push(state.data[i]);
                        }
                        state.data = tmp_list;
                        return;
                    }
                    else {
                    invoke("util.throw_ex", 
                        "invalid position for list.insert: " + pos);
                    }
                    this.save_state(state);
                }),
                remove: my.fun.op(function (pos) {
                    var i, tmp_list,
                        state = this.get_state();
                    if (pos < 0 || pos >= state.data.length) {
                        invoke('util.throw_ex', 'invalid position for list.remove: '+pos);
                    }
                    if (pos === (state.data.length - 1)) {
                        // append to end of list
                        state.data.pop();
                    } else {
                        tmp_list = state.data.slice(0,pos);
                        for (i = pos + 1; i < state.data.length; i++) {
                            tmp_list.push(state.data[i]);
                        }
                        state.data = tmp_list;
                    }
                    this.save_state(state);
                }),
                get: my.fun.api(function (pos) {
                    var state = this.get_state();
                    if (pos < 0 || pos >= this.state.length) {
                        invoke('util.throw_ex', "invalid list position "+pos);
                    }
                    return this.get_object(state.data[pos]); 
                }),
                length: my.fun.api(function () {
                    return this.get_state().data.length;
                })
                // TODO: queue, stack API functions
            }, Object.create(my.common_base)]);
            my.base_objects.list.State = function (data) {
                this.data = data;
            };
            my.base_objects.list.State.prototype.clone = function() {
                var cloned_data = Array.prototype.slice.apply(this.data)
                return new my.base_objects.list.State(cloned_data);
            };
 
        /* Create constructors for data types.
         * spec.oid, spec.type and spec.postlock_instance must be set!
         */
        my.fun.cons = {
            make: function(spec) {
                var obj = Object.create(my.base_objects[spec.type]).init(spec);
                spec.postlock_instance.state.save_object(obj);
                return obj;
            },
            make_with_create_op: function(spec) {
                return spec.postlock_instance.state.run_in_transaction(function () {
                    // 1. create the object first
                    var obj = my.fun.cons.make(spec);
                    // 2. add the 'create' op to the transaction
                    spec.postlock_instance.state.current_transaction().add_op({
                        cmd: 'create',
                        params: [{
                            oid: spec.oid,
                            type: spec.type,
                            state: spec.state // important for 'data' objects
                        }]
                    });
                    return obj;
                });
            }
        };
        return my.fun.cons;
    }()));
}());
