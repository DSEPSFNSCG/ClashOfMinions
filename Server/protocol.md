# Example Exchange

Client -> Server
```json
{
  "type" : "newGame",
  "name" : "paul",
}
```

Server -> Client
```json
{
  "type" : "gameStart",
  "otherName" : "betty",
  "youStart" : true,
  "gameId" : 1,
  "token" : "caohlurcdailcga"
}
```

Client -> Server
```json
{
  "type" : "place",
  "contents": {
    "position": [3,5],
    "stats": {
      "attackdmg":0,
      "attackrange":0,
      "buffrange":0,
      "healing":0,
      "atkbuff":0,
      "healbuff":0,
      "shield":0,
      "maxhealth":0
    }
  }
}
```

Server -> Client
```json
{
    "type" : "placeSuccess"; // or "invalidPlacing"
}
```

Server -> other Client
```json
{
  "type":"otherPlayerPlaced",
  "contents":{
    "position":[7,3],
    "stats":{
      "attackdmg":0,
      "attackrange":0,
      "buffrange":0,
      "healing":0,
      "atkbuff":0,
      "healbuff":0,
      "shield":0,
      "maxhealth":0
    }
  }
}
```

# Restore game example

Client -> Server

```json
{
  "type" : "restoreRequest",
  "gameId" : 1,
  "token" : 504351564210654210,
  "getHistoryFrom" : 10,
}
```

```json
{
  "type" : "restoreSuccess",
  "contents" : [
    {
      "position": [3,5],
      "stats" : {
        "boosthealrange" : 3,
        "attackrange" : 2,
        "health" : 4,
        "attackdmg" : 2,
        "healstrength" : 1
      }
    },
    {
      "position": [3,5],
      "stats" : {
        "boosthealrange" : 3,
        "attackrange" : 2,
        "health" : 4,
        "attackdmg" : 2,
        "healstrength" : 1
      }
    },
  ]
}
```


There can be any number of responses like this:

```json
{
  "type" : "logResponse",
  "contents" : "some arbitrary message"
}
```
