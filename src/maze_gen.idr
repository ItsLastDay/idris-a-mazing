module Main

import Data.Vect
import Data.Fin

import Effects
import Effect.Exception
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


genLoop : Eff () [STDIO, SYSTEM, RND, EXCEPTION String]
genLoop = do
  [prog, n_str, m_str] <- getArgs | [] => putStrLn "Can't happen"
                          | [prog] => putStrLn "No arguments"
                          | [prog, n] => putStrLn "Not enough arguments"
                          | _ => putStrLn "Too many arguments" 
  
  -- Interesting fact: if we cast to Nat,
  -- and then check that, e.g., `n <= 0`,
  -- negative numbers will *pass this check!*.  
  -- However, in REPL, `the Nat (cast "-4")` gives 0.
  let n = the Integer (cast n_str)
  let m = the Integer (cast m_str)
  if n <= 0 || m <= 0
     then raise "Invalid maze parameters"
     else pure ()
  
  t <- time
  srand t  
                          
  -- generate entry column (0..m-1)
  enter <- rndInt 0 (m - 1) 
  -- generate exit column (0..m-1)
  exit <- rndInt 0 (m - 1)


  pure ()

main : IO ()
main = run genLoop
