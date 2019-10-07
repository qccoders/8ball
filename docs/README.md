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

# Agent API

## Handling a Question

The agent API for handling questions is identical to the hub, except for the addition of the `X-Forwarded-For` header used to identify the client that originated the request at the hub.

### Request

```
GET /answer?q=<your question>&ttl=<optional request ttl>
X-Forwarded-For: <originating IP address>
```

### Response

* (Optional) Rate Limited: HTTP 429/Too Many Requests
* (Optional) IP Ban: HTTP 403/Forbidden
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

## Agent Registration

In order to register your agent with the hub, you must add an entry to `config/agents.yml`, like so:

```yaml
- name: Stooges
  webhook: https://4f9gs2dqw3.execute-api.us-east-1.amazonaws.com/prod/
  language: nodejs
```

Where `name` is the name for your agent (that's up to you), `webhook` is the fully qualified URL where your agent will receive question requests, and `language` is the language in which your agent is written (nodejs, c#, go, python, etc.. this is so we can add an icon to the UI).