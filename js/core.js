/* Postlock javascript client
 * core.js:
 * 1. registers global postlock variable
 */

(function (global_scope) {
    var my = {
        values: {},
        fun: {
            name_to_list: function(name) {
                var l = name.split(".");
                return l;
            },
            list_to_name: function(list) {
                var n = list.join(".");
                return n;
            },
            set: function(name, value) {
                var o = my.values, i, l = my.fun.name_to_list(name);
                for (i = 0; i < (l.length - 1); i++) {
                    if (!(l[i] in o)) o[l[i]] = {};
                    o = o[l[i]];
                }
                o[l[l.length - 1]] = value;
            },
            get: function(name) {
                var o = my.values, i, l = my.fun.name_to_list(name || "");
                for (i = 0; i < l.length; i++) {
                    if (!(l[i] in o)) throw {message: name +" is not defined"};
                    o = o[l[i]];
                }
                return o;
            },
            make_new: function(spec) {
                return my.fun.get("modules.instance")(spec);
            }
        }
    };
    if (!('POSTLOCK' in global_scope)) {
        global_scope['POSTLOCK'] = {
            set: my.fun.set,
            // note: return undefined if key is undefined
            get: my.fun.get,
            make_new: my.fun.make_new 
        }
    } 
})(window || this);
