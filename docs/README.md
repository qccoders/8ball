# Registration

    <- POST /webhooks header: { Authorization: Bearer <static API token> }, body: <url to agent>

# (De)Registration

    <- DELETE /webhooks/<url to agent>

    note: if your agent fails to respond to 3 requests in a row, it is automatically unregistered.
    
# Request lifecycle

user -> GET /answer?q=blah?&ttl=5
hub -> 
    foreach (registered webhook) -> 
        GET /answer?q=blah?&ttl=5
    <- 
    {
        name: 'name',
        response: 0-19,
        delay: 0ms
        children: [{
            name: 'name',
            response: 0-19
            delay: <ms to respond to parent>
            children: [..]
        }]
    }

