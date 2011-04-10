(function() {
if (POSTLOCK) POSTLOCK.set("modules.state_storage", function(spec) {
    // POSTLOCK state storage module's code.
    // This module is responsible for:
    // - storing/retrieving objects
    // - adding objects from another state_storage instance
    // - applying 'meta object operations' (create, delete).
    var my = {
        objects: {},
        fun: {
            // ---- functions for object storage/retrieval ----
            get: function (oid) {return my.objects[oid];},
            set: function (oid, obj) {my.objects[oid] = obj;},
            // ---- ----
        } // end my.fun
    };
    my.exports = {
    };
    return my.exports;
}); 
})();
 
