# Hub API

## Asking a Question

### Request

```
GET /answer?q=<your question>&ttl=<optional request ttl>
```

The `q` query parameter contains the question you'd like to ask.

The optional `ttl` query parameter specifies the maximum time, in milliseconds, to wait for responses from the network before completing the request.  The minimum value is 1000, or 1 second.

### Response

* Rate Limited: HTTP 429/Too Many Requests
* IP Ban: HTTP 403/Forbidden
* Success: HTTP 200/OK

```
{
    name: <string>,
    response: <integer, 0-19>,
    delay: <integer, optional>
    children: [{
        name: <string>,
        response: <integer, 0-19>,
        delay: <integer, optional>
        children: [..]
    }]
}
```

The `name` property should identify the source of the response.  The top level value should match the name associated with the agent's API key, but the hub will make this substitution if it doesn't.  Any children names are up to the agent.

The `response` property must be an integer that corresponds to one of the [official Magic 8-Ball responses](https://en.wikipedia.org/wiki/Magic_8-Ball):

Integer | Response
--- | ---
0 | It is certain.
1 | It is decidedly so.
2 | Without a doubt.
3 | Yes - definitely.
4 | You may rely on it.
5 | As I see it, yes.
6 | Most likely.
7 | Outlook good.
8 | Yes.
9 | Signs point to yes.
10| Reply hazy, try again.
11| Ask again later.
12| Better not tell you now.
13| Cannot predict now.
14| Concentrate and ask again.
15| Don't count on it.
16| My reply is no.
17| My sources say no.
18| Outlook not so good.
19| Very doubtful.

The `delay` property can optionally be specified to inform the hub of the time taken to fetch the response from children.  The top level value will be replaced by the hub.  This is used to track latency in the network, and is informational only.

### Example

```
curl -XGET 'http://8ball.qccoders.org/answer?q=yes or no question&ttl=5000'

---------------------------------------------------------------------------

[
  {
    "name": "Agent 007",
    "response": 5,
    "delay": 187,
    "children": [
      {
        "name": "Moonraker",
        "response": 13,
        "delay": 123
      },
      {
        "name": "GoldenEye",
        "response": 4,
        "delay": 123,
        "children": [
          {
            "name": "The Movie",
            "response": 13,
            "delay": 78
          },
          {
            "name": "The N64 Game",
            "response": 3,
            "delay": 52
          }
        ]
      },
      {
        "name": "Skyfall",
        "response": 8,
        "delay": 157
      }
    ]
  },
  {
    "name": "Simple Agent",
    "response": 10,
    "delay": 30
  }
]
```

The example response payload above contains responses from two first-order agents, "Agent 007", and "Simple Agent".  The agent "Agent 007" has three child agents, "Moonraker", "GoldenEye", and "Skyfall", and the "GoldenEye" agent has two additional children, "The Movie" and "The N64 Game".

The "GoldenEye" agent is responsible for populating the `delay` property of its two children responses, and the "Agent 007" agent is responsible for populating the values for each of its three children.  The hub then calculates the delay for the "Agent 007" response.

Agents which have child agents are responsible for determining the agent's overall `response`, which might be a weighted average of child responses, or something completely random; this is up to the agent.

## Agent Registration

Your agent must register its callback URL with the hub to begin receiving questions.

It might be a good idea to re-register if you haven't recieved a question from the hub in a while, as your registration may have been removed or lost.

Use this endpoint to check to see if the hub is operational.  If it returns a 200/OK response, it's up.  Otherwise, it's down.

### Request 

```
PUT /webhooks/<url to agent>
Authorization: Bearer <static API token>
```
### Response

* Authorization Failure: HTTP 403/Forbidden
* Success: HTTP 200/OK

### Example

```
curl -XPUT -H 'Authorization: Bearer 700373e4-1214-4b12-8190-53e8fc54426b' 'http://8ball.qccoders.org/webhooks/http%3A%2F%2Fmy-agent%3A80%2Fapi'
```

## Agent Deregistration

As your agent is going offline, it would be nice if you would remove the registration so the hub doesn't continue to try to send questions to it.

If your agent fails to respond to 3 requests in a row, it is automatically unregistered.  This doesn't include timeouts.

### Request

```
DELETE /webhooks/<url to agent>
Authorization: Bearer <static API token>
```

### Response

* Authorization Failure: HTTP 403/Forbidden
* Success: HTTP 204/No Content

### Example

```
curl -XDELETE -H 'Authorization: Bearer 700373e4-1214-4b12-8190-53e8fc54426b' 'http://8ball.qccoders.org/webhooks/http%3A%2F%2Fmy-agent%3A80%2Fapi'
```

# Agent API

## Forwarding a Question

The agent API for handling questions is identical to the hub, except for the addition of the `X-Forwarded-For` header used to identify the client that originated the request at the hub.

### Request

```
GET /answer?q=<your question>&ttl=<optional request ttl>
X-Forwarded-For: <originating IP address>
```

### Response

* Rate Limited: HTTP 429/Too Many Requests
* IP Ban: HTTP 403/Forbidden
* Success: HTTP 200/OK

```
{
    name: <string>,
    response: <integer, 0-19>,
    delay: <integer, optional>
    children: [{
        name: <string>,
        response: <integer, 0-19>,
        delay: <integer, optional>
        children: [..]
    }]
}
```