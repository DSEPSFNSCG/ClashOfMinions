
# Setup

Client -> Server, looking for new game

```json
{
  "type" : "searchGame",
  "name" : "paul",
}
```

can have same username

if 2 players found, Server -> Client, game found

```json
{
  "type" : "gameFound",
  "otherPlayername" : "betty",
  "canStart" : true,
  "gameId" : 1,
  "token" : "rCHurocahulraoceducraogedu",
}
```
Token is unique and random per player and persistent

# Place Unit

Client -> Server

```json
{
  "type" : "placeRequest",
  "position" : {
    "x" : 3,
    "y" : 5,
  },
  "stats" : {
    "boosthealrange" : 3,
    "attackrange" : 2,
    "health" : 4,
    "attackdmg" : 2,
    "healstrength" : 1,
  },
}
```

Server -> Client that requested unit placement

```json
{
  "type" : "placeReply",
  "success" : true,
  "error" : "unit already in that place",
}
```
error is optional

Server -> Client that didn't place unit

```json
{
  "type" : "newHistoryItem",
  "id" : 0,
  "unit" : {
    "position" : {
      "x" : 3,
      "y" : 5,
    },
    "stats" : {
      "boosthealrange" : 3,
      "attackrange" : 2,
      "health" : 4,
      "attackdmg" : 2,
      "healstrength" : 1,
    },
  }
}
```

On game finished, server tears down game

# Restore game

Client -> Server

```json
{
  "type" : "restoreRequest",
  "gameId" : 1,
  "token" : "rcL42ueogdurocgdrC",
  "getHistoryFrom" : 10,
}
```
request history from 10th action
```json
{
  "type" : "restoreReply",
  "success" : true,
  "history" : [
    {
      "id" : 10,
      "unit" : {
        "position" : {
          "x" : 3,
          "y" : 5,
        },
        "stats" : {
          "boosthealrange" : 3,
          "attackrange" : 2,
          "health" : 4,
          "attackdmg" : 2,
          "healstrength" : 1,
        },
      }
    }
    {
      "id" : 11,
      "unit" : {
        "position" : {
          "x" : 3,
          "y" : 5,
        },
        "stats" : {
          "boosthealrange" : 3,
          "attackrange" : 2,
          "health" : 4,
          "attackdmg" : 2,
          "healstrength" : 1,
        },
      }
    }
  ]
}

```
if false, history.size() == 0

# ToDo

- Is gameId needed? (no, but it's nice)

