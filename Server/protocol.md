
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
	// Token is unique and random per player and persistent
	"token" : "rCHurocahulraoceducraogedu",
}
```

# Place Unit

Client -> Server

```json
{
	"type" : "placeUnit",
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

Server -> Client that placed unit

```json
{
	"type" : "placeSuccess",
	"success" : true,
	(potential) "error" : "unit already in that place",
}
```

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
	"getHistoryFrom" : 10, // request history from 10th action
}
```

```json
{ 
	"type" : "restoreReply",
	"success" : true,
	// if false, history.size() == 0
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

# ToDo

- Is gameId needed? (no, but it's nice)

