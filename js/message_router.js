(function() {
if (POSTLOCK) POSTLOCK.set("modules.message_router", function(spec) {
    /* Postlock JS client message router module.
     * Responsibilities:
     * - register message routes based on from: and type: fields.
     * - deliver messages based on available routes.
     * - fire callbacks on error events.
     * 
     * expected spec format:
     * spec = {
     *     cb: [callback manager object]
     *     
     * }
     */
    var my = {
        any_field: spec.any_field || '__any__',
        fields: spec.fields || ['from', 'type'],
        // routes are stored as [field1][field2] = [dest1, dest2, ...], eg:
        routes: {},
        fun: {
            get_destination_list: function(selector) {
            // saves a message route.
            // selector should have at least one of
            // the 
                var current_parent = my.routes,
                    fieldname,
                    i = 0;
                for (; i < my.fields.length; i++) {
                    if (my.fields[i] in selector) {
                        fieldname = selector[my.fields[i]];
                    } else {
                        fieldname = my.any_field;
                    }
                    if (!(fieldname in current_parent)) {
                        current_parent[fieldname] = (i == (my.fields.length-1))?[]:{};
                    }
                    current_parent = current_parent[fieldname];
                }
                return current_parent;
            },
            add_route: function(selector, destination) {
                (my.fun.get_destination_list(selector)).push(destination);
            },
            remove_route: function(selector, destination) {
                var i, destination_list = my.fun.get_destination_list(selector);
                for (i = 0; i < destination_list.length; i++) {
                    if (destination_list[i] === destination) destination_list[i] = null;
                }
            },
            handle_incoming: function(msg) {
                var i, destinations = function() {
                        var j, ix, dest = my.routes;
                        for (j=0; j < my.fields.length; j++) {
                            if (msg[my.fields[j]] in dest) {
                                ix = msg[my.fields[j]];
                            } else {
                                ix = my.any_field;
                            }
                            if (ix in dest) {
                                dest = dest[ix];
                            } else {
                                return [spec.default_destination];
                            }
                        }
                        return dest;
                    }();
                for (i = 0; i < destinations.length; i++) {
                    if (typeof(destinations[i]) === 'function') destinations[i](msg);
                }
            }
        }
    };
    return {
        handle_incoming: my.fun.handle_incoming,
        add_route: my.fun.add_route,
        remove_route: my.fun.remove_route
    };
}); 
})();
 
