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
  "otherPlayername" : "betty",
  "canStart" : true,
  "gameId" : 1,
  "token" : 562234672464201 (64-bit unsigned integer)
}
```

Client -> Server
```json
{
  "type" : "place",
  "contents": {
    "position": [3,5],
    "stats": {
      "health" : 4,
      
      "boosthealrange" : 3, (not implemented right now, but coming)
      "attackrange" : 2, (same)
      "attackdmg" : 2, (same)
      "healstrength" : 1, (same)
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
  "type" : "placement";
  "contents": {
    "position": [3,5],
    "stats": {
      "health" : 4,
      "boosthealrange" : 3,
      "attackrange" : 2,
      "attackdmg" : 2,
      "healstrength" : 1,
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
        "healstrength" : 1,
    },
    {
      "position": [3,5],
      "stats" : {
        "boosthealrange" : 3,
        "attackrange" : 2,
        "health" : 4,
        "attackdmg" : 2,
        "healstrength" : 1,
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
