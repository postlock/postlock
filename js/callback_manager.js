(function() {
if (POSTLOCK) POSTLOCK.set("modules.callback_manager", function (spec) {
    // format of spec:
    // {
    //      string name (optional)
    // }
    // MODULE PRIVATE DATA:
    var instance = this,
        invoke = function(fun, args) {
            var a = args || [], real_args = ('length' in a)?a:[a];
            return POSTLOCK.get(fun).apply(instance, real_args);
	    },
    	my = {
        async_delay: 0,
        id: spec.name || "[unnamed object]",
        // debug state:
        debug: false,
        // callbacks holds 'signal' -> handler() entries.
        // postlock.js internal callbacks
        internal_cb: {},
        // user-defined callbacks
        user_cb: {},
        // default callback function:
        default_callback: function (signal, args) {
                if (my.debug) {
                    console.log("Callback manager for " + my.id + " received signal " + signal + " with arguments " + args + ".");
                }
                return;
        },
        fire: function (signal, args) {
                // apply the internal callback first
                var result = {}, args_array = invoke("util.args2array", [args]), first_cb, mapped_signal;
                if (signal in my.internal_cb) first_cb = my.internal_cb[signal];
                else {
                    first_cb = function() {return my.default_callback(signal, invoke("util.args2array", arguments));};
                }
                result.internal = first_cb.apply(instance, args_array) || {};
                // the internal callback may change the signal for the user-defined callbacks
                mapped_signal = result.internal.user_signal || signal;
                // apply the user-defined cb if one exists
                if (!result.internal.skip_user_cb && (mapped_signal in my.user_cb)) {
                    if (result.internal.user_cb_data) args_array.push(result.internal.user_cb_data);
                    result.user = my.user_cb[mapped_signal].apply(instance.exports, args_array);
                }
                return result;
        }
    }; // END my    
    my.exports = {
            debug: function (new_debug_state) {my.debug = new_debug_state;},
            set_internal_cb: function (signal, handler) {my.internal_cb[signal] = handler;},
            get_internal_cb: function (signal) {return my.internal_cb[signal];},
            set_user_cb: function (signal, handler) {my.user_cb[signal] = handler;},
            get_user_cb: function (signal) {return my.user_cb[signal];},
            remove_internal_cb: function (signal) {delete my.internal_cb[signal];},
            remove_user_cb: function (signal) {delete my.user_cb[signal];},
            fire: my.fire, 
            fire_async: function (signal, args) {setTimeout(function () {my.fire(signal, args);}, my.async_delay);},
            // used when we need a function which fires a signal. For example,
            // ajax-based callbacks or ws calls.
            wrap_signal: function (signal) {
                return function () {my.fire(signal, arguments);}
            } 
    }
    // MODULE EXPORTS
    return my.exports;
});
})();

