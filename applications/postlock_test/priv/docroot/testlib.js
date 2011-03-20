// assumes jQuery
var get_backend_url = function() {
    var path = document.location.href.slice(7).split("/");
    path = path.slice(0, path.length -1);
    var backend_url = "ws://" +  path.join("/") + "/backend.yaws";
    return backend_url;
}
var connect_clients = function(spec) {
    // spec.users: [{username: ,password:}]
    // spec.url: backend url
    // spec.on_all_connected
    // spec.on_failure
    var i, clients = [], current_client;
    $(spec.users).each(function() {
        clients.push({
            postlock: null, 
            connected: false, 
            username: this.username, 
            password: this.password,
            error: false
        });
    });
    $(clients).each(function() {
        var current_client = this;
        current_client.postlock = POSTLOCK.make_new({
            url: spec.url, 
            username: current_client.username, 
            password: current_client.password
        }).set_cb('ws_onclose', function() {
            current_client.error = true;
            spec.on_failure(clients);
            return; // do not continue with connection attempts
        }).set_cb('connected', function() {
            current_client.connected = true;
            var all_connected = true;
            $(clients).each(function() {
                all_connected = all_connected && this.connected;
            });
            if (all_connected) spec.on_all_connected(clients);
        }).connect();
    });
    return clients;
};
