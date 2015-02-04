# Racket-go

```
   A B C D E F G H I J K L M N O P Q R
19 . . . . . . . . . . . . . . . . . . 19
18 . . . . . . . . . . . . . . . . . . 18
17 . . . . . . . . . . . . . . . . . . 17
16 . . . + . . . . . + . . . . + . . . 16
15 . . . . . . . . . . . . . . . . . . 15
14 . . . . . . . . . . . . . . . . . . 14
13 . . . . . . . . . . . . . . . . . . 13
12 . . . . . . . . . . . . . . . . . . 12
11 . . . . . . . . . . . . . . . . . . 11
10 . . . + . . . . . + . . . . + . . . 10
 9 . . . . . . . . . . . . . . . . . .  9
 8 . . . . . . . . . . . . . . . . . .  8
 7 . . . . . . . . . . . . . . . . . .  7
 6 . . . . . . . . . . . . . . . . . .  6
 5 . . . . . . . . . . . . . . . . . .  5
 4 . . . X . . . . . + . . . . + . . .  4
 3 . . . . . . . . . . . . . . . . . .  3
 2 . . . . . . . . . . . . . . . . . .  2
 1 . . . . . . . . . . . . . . . . . .  1
   A B C D E F G H I J K L M N O P Q R

Enter move Black: D4
```

# Usage:

```
go [ <option> ... ]
 where <option> is one of
  -l <file>, --load <file> : Load save file to continue previous game
  --help, -h : Show this help
  -- : Do not treat any remaining argument as a switch (at this level)
 Multiple single-letter switches can be combined after one `-'; for
  example: `-h-' is the same as `-h --'
```

# What works:
  - robust move parsing
  - alternating turns
  - saving/loading
  - metacommands ex. pass, save, exit
  - collision detection

# TODO:
  - add group/capture logic
    game, changing names of players, etc.
  - A.I. (at least rudimentary)
