# Racket-go

```
   A B C D E F G H I J K L M N O P Q R S
19 . . . . . . . . . . . . . . . . . . . 19
18 . . . . . . . . . . . . . . . . . . . 18
17 . . . . . . . . . . . . . . . . . . . 17
16 . .(X)+ . . . . . + . . . . . + . . . 16
15 . . . . . . . . . . . . . . . . . . . 15
14 . . . . . . . . . . . . . . . . . . . 14
13 . . . . . . . . . . . . . . . . . . . 13
12 . . . . . . . . . . . . . . . . . . . 12
11 . . . . . . . . . . . . . . . . . . . 11
10 . . . + . . . . . + . . . . . + . . . 10
 9 . . . . . . . . . . . . . . . . . . .  9
 8 . . . . . . . . . . . . . . . . . . .  8
 7 . . . . . . . . . . . . . . . . . . .  7
 6 . . . . . . . . . . . . . . . . . . .  6
 5 . . . . . . . . . . . . . . . O . . .  5
 4 . . . X . . . . . + . . . . . + . . .  4
 3 . . . . . . . . . . . . . . . . . . .  3
 2 . . . . . . . . . . . . . . . . . . .  2
 1 . . . . . . . . . . . . . . . . . . .  1
   A B C D E F G H I J K L M N O P Q R S

Enter move Black: C16
```

# Usage:

```
main.rkt [ <option> ... ]
 where <option> is one of
  -l <file>, --load <file> : Load save file to continue previous game, default save location is ~/go.save
  -c, --computer : Play against a computer opponent (Just picks randomly for now)
/ -s, --server : [NOT IMPLEMENTED] Run as a server for someone to connect to
\ -r <ip>, --remote <ip> : [NOT IMPLEMENTED] Connect to ip of computer running racket-go as a server
  --help, -h : Show this help
  -- : Do not treat any remaining argument as a switch (at this level)
 /|\ Brackets indicate mutually exclusive options.
 Multiple single-letter switches can be combined after one `-'; for
  example: `-h-' is the same as `-h --'
```

# What works:
  - Complete Player vs. Player play including
    - robust move parsing
    - alternating turns
    - saving/loading
    - metacommands ex. pass, save, exit
    - collision detection
    - super-ko detection
    - suicide detection
    - capturing
    - track number of captured stones per player
    - game termination when both players pass consecutively
	- last played stone indicator (X)

# TODO:
  - changing names of players, etc.
  - A.I. (at least rudimentary)
  - Graphical interface
  - game timer
  - keep game tree to move forward/backward/laterally in game state
  - load and save smart game format files
