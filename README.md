# Maze generator written in Idris
Exam project for ["Programming with dependent types using Idris"](http://compsciclub.ru/courses/idrisprogramming/2017-spring/) course (held in Saint-Petersburg by Computer Science Club, lecturer -- [bravit](https://github.com/bravit)).

Tested under Idris 1.0.

# How to run
Example usage (generating 20x20 maze):
```
idris -p effects -o ./bin/mazegen ./src/maze_gen_v2.idr
./bin/mazegen 20 20
```


Example of generated maze (20x20):  
here `S` denotes start cell, `E` denotes end cell, `o` denotes cells on path from enter to exit,
`.` denotes a cell, `|` is a vertical wall, `_` is a horizontal wall.
```
. o o o|.|. . .|.|S o . . . . . . . .|.
_   _       _ _   _   _ _ _ _ _   _ _   
. o o|o . . .|.|.|o o . .|. . .|.|.|.|.
        _ _ _           _   _ _ _       
.|.|o|o o o|.|o o o|.|. . . . .|. . .|.
_     _         _ _   _ _ _ _     _ _   
. .|o .|.|o o o . .|. . . . .|. . .|. .
  _   _ _ _ _           _   _   _ _   _ 
. .|o o . . .|.|.|.|.|. .|.|.|. .|. . .
  _ _   _ _ _ _     _   _ _   _ _   _ _ 
. .|. o . . . .|.|.|. . .|.|. . .|.|. .
        _ _   _ _ _ _         _ _     _ 
.|.|.|o . .|.|.|.|.|.|.|. .|. .|. . . .
_       _ _ _         _ _ _   _   _ _ _ 
. .|.|o o . . . .|.|. .|. . . . . . . .
_         _   _ _     _ _   _       _ _ 
. .|.|.|o o|.|o o o .|. . .|.|.|.|. . .
_   _ _     _   _       _ _   _ _ _     
. .|. . .|o o o|. o|.|.|. . . . . .|.|.
  _ _     _     _   _       _   _       
.|. . .|.|. .|. .|o|.|.|.|.|. . .|.|.|.
_       _ _ _             _     _   _   
. .|.|. . .|. .|.|o . . . .|.|. .|.|.|.
  _   _ _       _     _ _ _ _ _   _   _ 
.|. .|.|.|.|.|.|. o|. . .|. .|. . .|.|.
_         _   _     _ _ _     _ _ _     
. .|. .|. . .|. .|o o|. . .|.|. .|. . .
_         _   _ _ _     _ _ _   _     _ 
. .|.|.|. .|.|o o o o . . . . .|. .|. .
_ _ _   _       _ _   _   _   _   _   _ 
. . . . .|.|.|o|. . .|. . .|. . . .|. .
  _     _ _ _             _ _           
.|.|.|. . .|o o|.|.|.|.|. . .|.|.|.|.|.
        _ _                 _     _ _   
.|. .|. .|. o|.|.|.|.|.|.|.|. .|. .|. .
    _     _   _ _ _ _         _   _     
.|.|. .|. .|o o o|. . .|.|.|. .|.|.|.|.
_   _   _ _ _ _   _ _   _   _ _     _   
. .|. . .|. . . E .|. . .|. .|. . . .|.
```
