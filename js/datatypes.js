(function () {
    if (!POSTLOCK) return;
    POSTLOCK.set("modules.datatypes", function (spec) {
        // Common base class for all postlock data types.
        var UNSAFE_PREFIX = "unsafe_",
            instance = this,
            invoke = function (fun, args) {
                var a = args || [], real_args = ('length' in a)?a:[a];
                return POSTLOCK.get(fun).apply(instance, real_args);
    	    },
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
            common_base = {
                // create exports object containing public functions
                // bound to the obj_instance
                make_exports: function (s) {
                    // spec determines which operations
                    // lack an unsafe_* variant. The format is:
                    // spec = {
                    //      no_unsafe_op: []
                    // }
                    var i, 
                        exports = {}, 
                        obj_instance = this, 
                        spec = s || {},
                        invoke_op: function (is_safe, op, args) {
                            // invoke an operation on the current object
                        };
                    // Export public functions under 'pub'.
                    // Our only job is to bind them to the obj_instance.
                    if (typeof(this.pub) === 'object') {
                        for (i in this.pub) {
                            if (this.pub.hasOwnProperty(i)) {
                                exports[i] = function () {
                                    var ix = i;
                                    return obj_instance.pub[ix](arguments);
                                }
                            }
                        }
                    }
                    // Export operation functions under 'op'.
                    // Besides the regular version, we also
                    // need to export and 'unsafe' version unless
                    // the spec argument indicates otherwise.
                    if (typeof(this.op) === 'object') {
                        for (i in this.op) {
                            if (this.op.hasOwnProperty(i)) {
                                // 'regular' version of operation
                                exports[i] = function () {
                                    var op = i;
                                    return invoke_op(true, op, arguments);
                                }
                                // 'unsafe' version of operation
                                if ( /* if spec has no list of no unsafe ops */
                                    !(spec.no_unsafe_op) ||
                                     /* or if it does but our op is not in it */
                                    spec.no_unsafe_op.indexOf(i) === -1) {
                                    exports[UNSAFE_PREFIX + i] = function () {
                                        var op = i;
                                        return invoke_op(false, op, arguments);
                                    }
                                }
                            }
                        }
                    }
                    return exports;
                },
                init: function (spec) {
                    this.oid = spec.oid;
                    var make_exports_spec;
                    if (this.priv && typeof(this.priv.init) === 'function') {
                        this.priv.init(arguments);
                        this.exports = this.make_exports(spec);
                    }
                    return this;
                },
                oid: function() {
                    return String(this.oid);
                }
            };
        // ---- Object type declarations ----
        return {
            data: function (spec) {
                return merge({
                    priv: {
                        init: function () {
                            this.state = null;
                        }
                    },
                    op: {
                        set: function (value) {
                            this.state = value;
                        }
                    },
                    pub: {
                        get: function () {
                            // reference to state returned.
                            // users should be smart enough not to change it...
                            return this.state; 
                        }
                    }
                }, Object.create(common_base)).init(spec);
            },
            dict: function (spec) {
                var my = {
                    prefix: "entry_"
                };
                my.fun = {
                    key2ix: function (key) {
                        return my.prefix + key;
                    },
                    ix2key: function (ix) {
                        return ix.substr(my.prefix.length);
                    }
                };
                return merge({
                    priv: {
                        init: function () {
                            this.state = {};
                        }
                    },
                    op: {
                        set: function (key, o) {
                            // o can be and oid or it can be an object
                            this.state[my.fun.key2ix(key)] = to_oid(o);
                        },
                        unset: function (key) {
                            var ix = my.fun.key2ix(key);
                            if (!this.state.hasOwnProperty(ix)) {
                                invoke("util.throw_ex", 'cannot unset missing key');
                            }
                            delete this.state[ix];
                        }
                    },
                    pub: {
                        get: function (key) {
                            // reference to state returned.
                            // users should be smart enough not to change it...
                            return to_obj(this.state[my.fun.key2ix(key)]); 
                        },
                        // note: the order of the keys is arbitrary.
                        keys: function () {
                            var i, keys = [];
                            for (i in my.state) {
                                if (my.state.hasOwnProperty(i)) {
                                    keys.push(my.fun.ix2key(i));
                                }
                            }
                            return keys;
                        }
                    }
                }, Object.create(common_base)).init(spec);
            },
            list: function (spec) {
                return merge({
                    priv: {
                        init: function () {
                            this.state = [];
                        }
                    },
                    op: {
                        insert: function (pos, o) {
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
                        },
                        remove: function (key) {
                            delete this.state[my.fun.key2ix(key)];
                        }
                    },
                    pub: {
                        get: function (key) {
                            // reference to state returned.
                            // users should be smart enough not to change it...
                            return this.state[my.fun.key2ix(key)]; 
                        },
                        // note: the order of the keys is arbitrary.
                        keys: function () {
                            var i, keys = [];
                            for (i in my.state) {
                                if (my.state.hasOwnProperty(i)) {
                                    keys.push(my.fun.ix2key(i));
                                }
                            }
                            return keys;
                        }
                    }
                }, Object.create(common_base)).init(spec);
            }
        };
    });
}());
