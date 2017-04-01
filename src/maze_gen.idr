module Main

import Data.Vect
import Data.Fin

import Effects
import Effect.Exception
import Effect.StdIO
import Effect.System
import Effect.State
import Effect.Random


shift_vec : List (Integer, Integer)
shift_vec = [(-1, 0), (0, 1), (0, -1), (1, 0)]

||| Vertex: a pair of coordinates on a grid.
Vertex : Type
Vertex = (Nat, Nat)

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
data MazeGrid : (n : Nat) -> (m : Nat) -> Type where
  Grid : (n : Nat) -> (m : Nat) -> (adj : List AdjList) -> MazeGrid n m


vertexToNat : Nat -> Nat -> Vertex -> Nat
vertexToNat n m x = (fst x) * m + (snd x)


natToVertex : Nat -> Nat -> Nat -> Vertex
natToVertex n m x = (x `div` m, x `mod` m)



generateGrid : (n : Nat) -> (m : Nat) -> Effects.SimpleEff.Eff (MazeGrid n m) [RND] 
generateGrid n m = do
    adjList <- genAdjList (n * m)
    pure $ Grid n m (reverse adjList)
  where genAdjList : (idx : Nat) -> Eff (List AdjList) [RND]
        genAdjList Z = pure []
        genAdjList (S idx) = do
          rest <- genAdjList idx
          cur <- genSingleAdj idx
          pure (cur :: rest)
    where genSingleAdj : Nat -> Eff (AdjList) [RND]
          genSingleAdj idx = do
            let v = natToVertex n m idx
            let x = the Integer $ cast $ fst v
            let y = the Integer $ cast $ snd v
            let lst = the (List (Integer, Integer)) [(dx + x, dy + y) | (dx, dy) <- shift_vec]
            let filtered = filter (\(a, b) => 0 <= a && a < (the Integer $ cast n) && 0 <= b && b < (the Integer $ cast m)) lst
            let casted = [(the Nat $ cast xx, the Nat $ cast yy) | (xx, yy) <- filtered] 
            pure casted


printMaze : MazeGrid n m -> Eff () [STDIO, EXCEPTION String]
printMaze (Grid n m adj) = do
      printRow 0
    where printRow : Nat -> Eff () [STDIO]
          printRow row_idx = do
            printColAtRow 0
            if row_idx + 1 == n 
               then putStrLn "End"
               else do
                 printColBetweenRows 0
                 printRow (row_idx + 1)
        where printColAtRow : Nat -> Eff () [STDIO]
              printColAtRow col_idx = do
                putChar '.'
                if col_idx + 1 == m
                   then putChar '\n'
                   else do
                     let adj_idx = vertexToNat n m (row_idx, col_idx)
                     if Prelude.List.elem (row_idx, col_idx + 1) (Prelude.List.index adj_idx {ok=believe_me True} adj)
                        then putChar ' '
                        else putChar '|'
                     printColAtRow (col_idx + 1)
              printColBetweenRows : Nat -> Eff () [STDIO]
              printColBetweenRows col_idx = do
                 let adj_idx = vertexToNat n m (row_idx, col_idx)
                 if Prelude.List.elem (row_idx + 1, col_idx) (Prelude.List.index adj_idx {ok=believe_me True} adj)
                    then putChar ' '
                    else putChar '-'
                 if col_idx + 1 == m
                    then putChar '\n'
                    else do
                      putChar ' '
                      printColBetweenRows (col_idx + 1)
                      


generatePrintMaze : Eff () [STDIO, SYSTEM, RND, EXCEPTION String]
generatePrintMaze = do
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

  -- generate random grid
  grid <- generateGrid (the Nat $ cast n) (the Nat $ cast m)

  printMaze grid

main : IO ()
main = run generatePrintMaze
