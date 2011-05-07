/* datatypes.js - defines postlock datatypes for use in js client api.
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
 * application needs to create a new postlock object and by state_storage
 * when it executes a transaction broadcast by the state server. In the
 * latter case, object creation does not need to be wrapped in a transaction
 * and the exported functions are never used. Setting spec.internal to true
 * will make the objects work this way.
 *
 * The code in the datatypes module is mostly independent of the postlock
 * instance. Unfortuantely, there are two exceptions to this rule:
 * 1. get() calls return objects instead of oid's
 * 2. transactions created by the module must be placed in the postlock
 *    instance's outgoing transaction queue.
 */
(function () {
    if (!POSTLOCK) return;
    POSTLOCK.set("modules.datatypes", function () {
        var instance = this,
            invoke = function (fun, args) {
                var a = args || [], real_args = ('length' in a)?a:[a];
                return POSTLOCK.get(fun).apply(instance, real_args);
            },
            my = {
                constants: {
                    UNSAFE_PREFIX = "unsafe_",
                    DICT_PREFIX = "entry_"
                },
                fun: {
                    merge = function (src, dst) {
                        var i;
                        if (typeof(src) !== 'object') return dst;
                        for (i in src) {
                            if (src.hasOwnProperty(i)) {
                                dst[i] = src[i];
                            }
                        }
                        return dst;
                    },
                    // takes an oid or an object and returns an oid
                    to_oid = function(o) {
                        if (typeof(o) === 'string') {
                            return String(o);
                        }
                        if (typeof(o) === 'object') &&
                            typeof(o.oid) === 'function) {
                            return o.oid();
                        }
                        invoke("util.throw_ex", 
                            "attempted to get oid of non-postlock object");
                    },
                    // takes an oid or an object and returns an object
                    to_obj = function(o) {
                        if (typeof(o) === 'string') {
                            return instance.state_storage.get(o);
                        }
                        return o;
                    },
                    // functions to add metadata to other functions
                    api = function(fun) {
                        fun.type = 'api';
                        return fun;
                    },
                    op = function(fun) {
                        fun.type = 'op';
                        fun.has_unsafe = true;
                        return fun;
                    },
                    op_no_unsafe = function(fun) {
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
                    },
                    is_transaction: function (t) {
                        return (typeof(t) === 'object' &&
                                typeof(t.execute) === 'function');
                    },
                    guarantee_transaction: function (fun, transaction) {
                        var transaction_needed = !my.fun.is_transaction(transaction);
                        // if no transaction is passed to us, 
                        // lets create one:
                        if (transaction_needed) {
                            transaction = invoke('modules.transaction');
                        }
                        fun(transaction, transaction_needed);
                        if (transaction_needed) {
                            transaction.execute();
                        }
                        return transaction;
                    }
                } // end my.fun
            }; // end my
            my.base_objects = {};
            my.base_objects.common_base = {
                // create exports object containing public functions
                // bound to the obj_instance
                make_exports: function () {
                    var i, 
                        exports = {}, 
                        obj_instance = this, 
                        add_op_to_transaction: function (is_safe, op, args) {
                            // 1. Adds the operation to the current transaction.
                            // 2. If the transaction is created here, executes it as well.
                            // 3. Returns the transaction.
                            var transaction;
                            var op_parameters = invoke('util.args2array', [args]);
                            if (args.length > 0 && 
                                my.fun.is_transaction(args[args.length - 1])) {
                                transaction = op_parameters.pop();
                            }
                            obj_instance[op].apply(obj_instance, op_parameters);
                            // if a transaction was provided by the user,
                            // it will be the last element in args
                            my.fun.guarantee_transaction(
                                function (t) {
                                    // add the operation to the transaction
                                    t.add_operation({
                                        oid: obj_instance.oid,
                                        cmd: op,
                                        safe: is_safe,
                                        params: op_parameters
                                    });
                                },
                                transaction
                            );
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
                                            obj_instance[ix](arguments);
                                        }
                                    }());
                                    break;
                                case 'op';
                                    // op functions call invoke_op.
                                    // depending on the has_unsafe field,
                                    // there may be an 'unsafe_' version
                                    // exported as well.
                                    (function () {
                                        var ix = i;
                                        exports[ix] = function() {
                                            return add_op_to_transaction(true, ix, arguments);
                                        };
                                        if (this[ix].has_unsafe === true) {
                                            exports[my.constants.UNSAFE_PREFIX + ix] = function() {
                                                return add_op_to_transaction(false, ix, arguments);
                                            };
                                        }
                                    }());
                                    break;
                                default:
                                    // this shouldn't ever happen
                                    console.error('unexpected function type');
                            }
                        }
                    }
                    return exports;
               },     
               init: function (spec, transaction) {
                    this.oid = spec.oid;
                    this.exports = {};
                    if (spec.internal !== true) {
                        this.exports = this.make_exports();
                        transaction = my.fun.guarantee_transaction(
                            function (t) {
                                t.add_operation({
                                    cmd: 'create',
                                    params: [{
                                        oid: obj_instance.oid,
                                        type: obj_instance.type,
                                    }]
                                });
                                // The init data function has access to the create transaction
                                // so it can add futher operations if necessary.
                                if (obj_instance.init_data) obj_instance.init_data(spec, t);
                            },
                            transaction
                        );
                        // transaction is saved to spec if the caller
                        // needs access to it.
                        spec.transaction = transaction;
                    }
                    return this;
               },
               oid: my.fun.api(function() {
                    return String(this.oid);
               })
            };
            /* The prototypes for the different data types follow.
             * Each prototype inherits from common_base, but is extended
             * with its own functions.
             */
            my.base_objects.data = merge({
                type: 'data,
                init_data: function () {
                    this.state = null;
                }
                set: my.fun.op(function (value) {
                    this.state = value;
                }),
                get: my.fun.api(function () {
                    // reference to state returned.
                    // users should be smart enough not to change it...
                    return this.state; 
                })
            }, Object.create(my.base_objects.common_base));
            my.base_objects.dict = merge({ 
                type: 'dict',
                init_data: function () {
                    this.state = {};
                },
                set: my.fun.op(function (key, obj_or_oid) {
                    // o can be and oid or it can be an object
                    this.state[my.fun.key2ix(key)] = to_oid(obj_or_oid);
                }),
                unset: my.fun.op(function (key) {
                    var ix = my.fun.key2ix(key);
                    if (!this.state.hasOwnProperty(ix)) {
                        invoke("util.throw_ex", 'cannot unset missing key');
                    }
                    delete this.state[ix];
                }),
                get: my.fun.api(function (key) {
                    // returns obj, not oid
                    var ix = my.fun.key2ix(key);
                    if (!this.state.hasOwnProperty(ix)) {
                        invoke("util.throw_ex", 'dict: key "'+key+'" is unbound');
                    }
                    return to_obj(this.state[ix]); 
                }),
                get_raw: function (key) {
                    return this.state[my.fun.key2ix(key)]; 
                },
                // note: the order of the keys is arbitrary.
                keys: my.fun.api(function () {
                    var i, keys = [];
                    for (i in my.state) {
                        if (my.state.hasOwnProperty(i)) {
                            keys.push(my.fun.ix2key(i));
                        }
                    }
                    return keys;
                })
            }, Object.create(my.base_objects.common_base));
            my.base_objects.list = merge({
                type: 'list',
                init_data: function () {
                    this.state = [];
                },
                // inserts cannot lead to data loss, no
                // unsafe_insert necessary.
                insert: my.fun.op_no_unsafe(function (pos, o) {
                    var i, 
                        oid = to_oid(o),
                        tmp_list;
                    if (pos === my.state.length) {
                        // append to end of list
                        my.state.push(value);
                        return;
                    }
                    if (pos < my.state.length) {
                        // move later elements back
                        tmp_list = my.state.slice(0,pos);
                        tmp_list.push(oid);
                        for (i = pos; i < my.state.length; i++) {
                            tmp_list.push(my.state[i]);
                        }
                        my.state = tmp_list;
                        return;
                    }
                    invoke("util.throw_ex", 
                        "invalid position for list.insert: " + pos);
                }),
                remove: my.fun.op(function (pos) {
                    delete this.state[my.fun.key2ix(key)];
                }),
                get: my.fun.api(function (pos) {
                    // reference to state returned.
                    // users should be smart enough not to change it...
                    if (pos < 0 || pos >= this.state.length) {
                        invoke('util.throw_ex', "invalid list position");
                    }
                    return to_obj(this.state[pos]); 
                }),
                get_raw: function (pos) {
                   return this.state[pos]; 
                },
                length: my.fun.api(function () {
                    // reference to state returned.
                    // users should be smart enough not to change it...
                    return this.state.length; 
                })
                // TODO: queue, stack API functions
           }, Object.create(my.base_objects.common_base));

        // ---- datatypes API: used to make new instances of data types ----
        // Note that these functions are not meant to be exposed to the user because
        // 1. the assume spec.oid is valid
        // 2. return the instance, when the user should receive instance.exports
        return {
            data: function (spec, transaction) {
                return Object.create(my.base_objects.data).init(spec, transaction);
            },
            dict: function (spec, transaction) {
                return Object.create(my.base_objects.dict).init(spec, transaction);
            },
            list: function (spec, transaction) {
                return Object.create(my.base_objects.list).init(spec, transaction);
            }
        };
    });
}());
