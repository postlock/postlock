(function() {
if (POSTLOCK) POSTLOCK.set("modules.counter", 
    function (initial_val) {
        // From Javascript - The Good Parts
        // Produce an object that produces unique strings. A
        // unique string is made up of two parts: a prefix
        // and a sequence number. The object comes with
        // methods for setting the prefix and sequence
        // number, and a gensym method that produces unique
        // strings.
        var prefix = '';
        var seq = initial_val || 0;
        return {
            get_prefix: function () {return prefix + '';},
            set_prefix: function (p) {
                prefix = String(p);
            },
            set_seq: function (s) {
                seq = s;
            },
            get_id: function (  ) {
                var result = prefix + seq;
                seq += 1;
                return result;
            } 
        };
    });
})();
