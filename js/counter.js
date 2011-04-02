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
        var seq = initial_val || 0;
        return {
            set_value: function (s) {
                seq = s+0;
            },
            get_value: function () {
                var result = seq + 0;
                seq += 1;
                return result;
            } 
        };
    });
})();
