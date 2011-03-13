/* util.js: utility functions.
 * note: this is assumed to be set 
 * to the calling postlock instance.
 */
(function() {
if (!POSTLOCK) return;
var util = {
    fire: function(oid, signal, args) {
        return this.cb.fire(signal,args);
    },
    require_transaction: function (fun, t, t_spec) {
        var ret = {}, apply_t = false;
        if (!util.is_transaction.apply(this,[t])) {
            apply_t = true;
            t = POSTLOCK.get("modules.transaction").apply(this, [t_spec]);
        }
        ret = {value: fun(t), transaction: t};
        if (apply_t) t.apply();
        return ret;
    },
    // from: http://www.javascriptkit.com/javatutors/arraysort.shtml
    numeric_sort: function (a, b) {
        return (a - b);
    },
    add_message_id: function (header) {
        header.id = PL.counters.client_message_id.getint();
        header.last_server_message_id = this.data.counters.last_server_message_id;
    },
    // Assembles ack's for applied remote transactions
    // based on the contents of PL.transactions.remote
    prepare_acks_of_remote_transactions: function () {
        var i, acknowledged_remote_transactions = PL.transactions.list_acked_remote();
        for (i=0; i < acknowledged_remote_transactions.length; i++) PL.transactions.remove(acknowledged_remote_transactions[i]);
        return acknowledged_remote_transactions;
    },
    is_array: function (value) {
        return value &&
            typeof value === 'object' &&
            value.constructor === Array;
    },
    args2array: function (args) {
        args = args || [];
        return Array.prototype.slice.apply(args);
    },
    throw_ex: function (message, extra_data) {
        var e = new Error(message),
            instance = this;
        if (extra_data !== undefined) e.data = extra_data;
        e.instance = instance;
        throw e;
    },
    is_transaction: function (t) {
        return typeof t === "object" && typeof t.get_tid === "function";
    },
    // from: http://stackoverflow.com/questions/122102/what-is-the-most-efficient-way-to-clone-a-javascript-object
    clone: function (from) {
        if (from == null || typeof from != "object") return from;
        if (from.constructor != Object && from.constructor != Array) return from;
        if (from.constructor == Date || from.constructor == RegExp || from.constructor == Function ||
            from.constructor == String || from.constructor == Number || from.constructor == Boolean)
            return new from.constructor(from);
        to = new from.constructor();
        for (var name in from) {
            to[name] = typeof to[name] == "undefined" ? this.extend(from[name], null) : to[name];
        }
        return to;
    },
    shallow_copy: function (src, dest) {
        var i;
        for (i in src) if (src.hasOwnProperty(i)) dest[i] = src[i];
        return dest;
    },
    get_timestamp: function () { return +new Date(); },
    retry_until: function (condition, cb_success, cb_failure, num_retries, timeout) {
        var ms = timeout || 10, 
            retries = num_retries || 5,
            on_failure = cb_failure || function () {util.throw_ex.apply(this,["retry_until failed", {args: util.args2array(arguments)}]);},
            do_try = function (r) {
                if (condition()) return {success: cb_success()};
                if (r === 0) return {failure: on_failure()};
                return {deferred: setTimeout(function () {do_try(r-1);},ms)};
            };
        return do_try(retries);
    },
    // from: http://stackoverflow.com/questions/1068834/object-comparison-in-javascript
    equals: function (t,x) {
        for(p in t) {
            if(typeof(x[p])=='undefined') {return false;}
        }
        for(p in t) {
            if (t[p]) {
                switch(typeof(t[p])) {
                case 'object':
                        if (!equals(t[p],x[p])) { return false }; break;
                case 'function':
                        if (typeof(x[p])=='undefined' || (p != 'equals' && t[p].toString() != x[p].toString())) { return false; }; break;
                default:
                        if (t[p] != x[p]) { return false; }
                }
            } else { 
            if (x[p]) { return false; }
            }
        }
        for(p in x) {
            if(typeof(t[p])=='undefined') {return false;}
        }
        return true;
    }
};
POSTLOCK.set("util", util); // end PL.util
})();
