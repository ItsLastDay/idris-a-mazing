# Maze generator written in Idris
Exam project for ["Programming with dependent types using Idris"](http://compsciclub.ru/courses/idrisprogramming/2017-spring/) course (held in Saint-Petersburg by Computer Science Club, lecturer -- [bravit](https://github.com/bravit)).

Tested under Idris 1.0.

# How to run
Example usage (generating 20x20 maze):
```
idris -p effects -o ./bin/mazegen ./src/maze_gen.idr
./bin/mazegen 20 20
```


Example of generated maze (20x20):  
here `S` denotes start cell, `E` denotes end cell, `o` denotes cells on path from enter to exit,
`.` denotes a cell, `|` is a vertical wall, `_` is a horizontal wall.
```
. . . . . . . .|.|. . .|. . .|.|o S . .
 | | | | | |_|_| |_| |_| |_|_| | |_|_| |
.|.|.|.|.|.|.|.|o o o o o o o o o|.|.|.
_| | |_| |_| | | |_|_|_| |_| | | | | |_|
. .|. .|. .|o o o . .|.|. .|.|.|. . .|.
_| |_| |_|_| | |_|_|_| |_|_|_|_|_|_| | |
. .|. . o o o|. . . . . .|. . .|. .|.|.
_| |_|_| |_| | | |_| | |_| | | | |_|_| |
. . .|o o .|.|.|. .|.|. . .|.|. . . . .
 | |_| |_| |_| |_| | | |_|_|_| | | | |_|
.|.|o o .|. .|.|.|.|.|. .|. . .|.|.|. .
 |_| |_|_| |_| | |_|_|_| |_|_| | | | |_|
.|. o o o|. .|. . .|. .|. . .|.|.|.|. .
 | |_|_| | | | |_| | |_|_|_| | |_| | |_|
.|.|. o o|.|.|. .|.|. .|. .|.|.|. .|. .
 |_| | |_|_| | |_|_| |_| |_| |_| |_| | |
. .|.|o|.|. .|. . . .|. . .|.|.|.|. .|.
 |_|_| | |_| |_|_| | | |_|_|_| |_|_| | |
.|. . o o|.|. .|. .|. . . . . . . .|.|.
_|_| |_| | | |_| | |_|_|_| | | | |_| | |
. . . .|o .|. .|.|. . .|. .|.|.|. .|.|.
_| | |_| |_|_|_| | |_|_|_| | |_| |_|_| |
.|.|. .|o o o|. .|.|.|.|. .|. .|. .|. .
 | |_|_|_|_| | |_| | | | |_|_| |_| |_| |
. . .|.|.|o o|.|. . . .|.|.|. . .|. .|.
_| | | | | | |_| | |_|_|_| | | |_|_|_| |
. .|.|o o o|. .|.|. . .|. .|.|. . . .|.
_| | | | | | | |_| | |_| | | |_| |_| | |
. .|.|o|.|.|.|.|. .|. . .|.|. .|. .|.|.
 | |_| | |_| |_|_| |_| | |_|_|_| |_|_| |
.|.|.|o|. .|. .|. .|. .|.|. . . .|.|.|.
_|_| | | |_| |_|_| | | | |_|_|_| | | |_|
. . o o|. .|.|.|. .|.|.|. . . .|. . . .
 | | | |_|_|_| | |_|_| | |_| | |_|_| |_|
.|.|o|.|. . .|. . .|. .|.|.|.|. .|. . .
_|_| |_| |_| |_| |_| | |_| |_|_|_|_| | |
. . o o . .|.|. . .|.|. .|. .|.|. .|.|.
 | | | |_|_|_|_| | |_| |_| |_| | |_|_| |
.|.|.|o o E|. . .|.|. . . . . . . . .|.

```

# Known issues
`mazegen 1 1` crashes with division by zero error. I do not divide anywhere, so I guess it is a compiler problem.  
Also, `mazegen 10 -5` (an similar negative numbers) somehow fails to check that the number is negative right away. Again, I think this is not a problem of my implementation - rather a compiler issue.
