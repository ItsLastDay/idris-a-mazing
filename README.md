# Maze generator written in Idris
Exam project for ["Programming with dependent types using Idris"](http://compsciclub.ru/courses/idrisprogramming/2017-spring/) course (held in Saint-Petersburg by Computer Science Club, lecturer -- [bravit](https://github.com/bravit)).

Tested under Idris 1.0.


Example of generated maze (30x30):  
here `X` denotes start and end cells, `o` denotes cells on path from enter to exit,
`.` denotes a cell, `|` is a vertical wall, `-` is a horizontal wall.
```
. o o o|.|. . .|.|o o . . . . . . . .|.
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
. .|. . .|. . . o .|. . .|. .|. . . .|.
```
