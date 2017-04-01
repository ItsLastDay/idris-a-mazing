module Main

import Data.Vect
import Data.Fin

import Effects
import Effect.StdIO
import Effect.System
import Effect.State
import Effect.Random


||| Vertex: a pair of coordinates on a grid.
Vertex : Type
Vertex = (Int, Int)

||| Adjacency list: neighbouring vertices.
AdjList : Type
AdjList = List Vertex

||| An n x m Maze is a graph. 
||| It has n * m vertices. 
||| Numeration goes from 0.
||| 
||| Example of 1x2 maze (along with indices of rows\columns):
|||       0 1 
|||         - 
|||  0   |. .|
|||       -
|||
data MazeGrid : Nat -> Nat -> Vect _ AdjList -> Type where
  Grid : (n : Nat) -> (m : Nat) -> (adj : Vect (n * m) AdjList) -> MazeGrid n m adj


genLoop : Eff () [STDIO, SYSTEM]
genLoop = do
  [prog, n, m] <- getArgs | [] => putStrLn "Can't happen"
                          | [prog] => putStrLn "No arguments"
                          | [prog, n] => putStrLn "Not enough arguments"
                          | _ => putStrLn "Too many arguments" 
                          
  pure ()


main : IO ()
main = run genLoop
