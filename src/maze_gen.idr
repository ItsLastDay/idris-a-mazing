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


printMaze : MazeGrid n m -> List Integer -> List Integer -> Vertex -> Vertex -> Eff () [STDIO]
printMaze (Grid n m adj) distToEnter distToExit enter exit = do
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
                let distEnterExit = Prelude.List.index {ok=believe_me True} (vertexToNat n m exit) distToEnter
                let curIdx = vertexToNat n m (row_idx, col_idx)
                let distanceCurEnter = Prelude.List.index {ok=believe_me True} curIdx distToEnter
                let distanceCurExit = Prelude.List.index {ok=believe_me True} curIdx distToExit
                {-
                putStr $ show curIdx
                putChar ' '
                putStr $ show distEnterExit
                putChar ' '
                putStr $ show distanceCurEnter
                putChar ' '
                putStr $ show distanceCurExit
                putChar '\n'
                -}
                if distEnterExit == distanceCurExit + distanceCurEnter
                   then putChar 'o'
                   else putChar '.'
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
      
calcDist : MazeGrid n m -> Vertex -> List Integer
calcDist (Grid n m adj) vert = take (n * m) $ bfs [vert] [if t == (vertexToNat n m vert) then 0 else -1 | t <- [0..n*m]]
    where bfs : List Vertex -> List Integer -> List Integer 
          bfs [] lst = lst
          bfs (x :: xs) lst = do
            let adj_idx = vertexToNat n m x
            let cur_val = Prelude.List.index adj_idx {ok=believe_me True} lst
            let neighbours = Prelude.List.index adj_idx {ok=believe_me True} adj
            let filtered_neighbours = Prelude.List.filter (\x => (Prelude.List.index (vertexToNat n m x) {ok=believe_me True} lst) == -1) neighbours
            bfs (xs ++ filtered_neighbours) (updLst lst filtered_neighbours (cur_val + 1))
        where updLst : List Integer -> List Vertex -> Integer -> List Integer
              updLst xs [] new_val = xs
              updLst xs (y :: ys) new_val = do
                let y_idx = vertexToNat n m y
                let sp = splitAt y_idx xs
                let sp_head = (fst sp)
                let sp_tail = (tail {ok=believe_me True} $ Prelude.Basics.snd sp)
                updLst (sp_head ++ (new_val :: sp_tail)) ys new_val

printAligned : Show a => Integer -> Integer -> List a -> Eff () [STDIO]
printAligned n m [] = putChar '\n'
printAligned n m (x :: xs) = do
  if (((Prelude.List.length xs) + 1) `mod` (the Nat $ cast m)) == 0
     then putChar '\n'
     else pure ()
  putStr $ show x
  putChar ' '
  printAligned n m xs


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

  let distToEnter = calcDist grid (0, (the Nat $ cast enter))
  let distToExit = calcDist grid ((the Nat $ cast (n - 1)), (the Nat $ cast exit))

  printMaze grid distToEnter distToExit (0, the Nat $ cast enter) (the Nat $ cast $ n - 1, the Nat $ cast exit)
  pure ()

main : IO ()
main = run generatePrintMaze
