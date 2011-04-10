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
        any_field: '__any__',
        fields: ['from', 'type'],
        cb: spec.cb || invoke("modules.callback_manager",
            {name:"main postlock object"}),
        // routes are stored as field.value.field.value, eg:
        // routes['from']['3']['type']['__any__'] = the_route
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
            }
            remove_route: function(selector, destination) {
                var destination_list = my.fun.get_destination_list(selector);
            }
        }
    };
    
}); 
})();
 
