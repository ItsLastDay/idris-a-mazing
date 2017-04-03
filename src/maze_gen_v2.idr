module Main

import Data.Vect
import Data.Fin

import Effects
import Effect.Exception
import Effect.StdIO
import Effect.System
import Effect.State
import Effect.Random


shift_vec : Vect 4 (Integer, Integer)
shift_vec = [(-1, 0), (0, 1), (0, -1), (1, 0)]


data Vertex : Nat -> Nat -> Type where
  MkVertex : (i : Fin n) -> (j : Fin m) -> Vertex n m


data GenericEdge : Type -> Type where
  MkEdge : a -> a -> GenericEdge a


GridEdge : Nat -> Nat -> Type
GridEdge n m = GenericEdge (Vertex n m)


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
  Grid : (n : Nat) -> (m : Nat) -> (edges : List (GridEdge n m)) -> MazeGrid n m


shiftByFin : Fin n -> Fin x -> Fin (x + n)
shiftByFin {n=Z} FZ y impossible
shiftByFin {n=(S k)} {x} FZ y = weakenN (S k) y
shiftByFin {n=Z} (FS a) b impossible
shiftByFin {n=(S k)} {x} (FS a) b = rewrite sym (plusSuccRightSucc x k) in shift 1 $ shiftByFin a b


mulFin : Fin n -> (m : Nat) -> Fin (m * n)
mulFin x Z = absurd $ mulFin x Z
mulFin {n} x (S Z) = rewrite plusZeroRightNeutral n in x
mulFin {n=Z} x (S k) impossible
mulFin {n=(S t)} x (S k) = rewrite plusCommutative t (k * (S t)) in 
                                   rewrite plusSuccRightSucc (k * (S t)) t in shiftByFin x (mulFin x k)
 

-- Every vertex is a pair (i, j), where i < n, j < m.
-- We can map pairs to natural numbers: i * m + j < (n - 1) * m + m - 1 = n * m.
vertexToFin : Vertex n m -> Fin (m * n + m)
vertexToFin {n} {m} (MkVertex i j) = shiftByFin j $ mulFin i m


rotate : Vect n a -> Nat -> Vect n a
rotate xs Z = xs
rotate {n=(S len)} (x :: xs) (S k) = rewrite plusCommutative 1 len in (rotate xs k) ++ [x]


rotateRandom : Vect n a -> Eff (Vect n a) [RND]
rotateRandom [] = pure []
rotateRandom xs = do
  idx <- rndInt 0 ((the Integer $ cast $ length xs) - 1)
  pure $ rotate xs (cast idx)
  

myNatToFin : (x : Nat) -> (up : Nat) -> LT x up -> Fin up
myNatToFin Z Z y impossible
myNatToFin Z (S k) y = FZ
myNatToFin (S k) Z y impossible
myNatToFin (S k) (S j) y = shift 1 $ myNatToFin k j (fromLteSucc y)


createVertex : (n : Nat) -> (m : Nat) -> (i : Nat) -> (j : Nat) ->
               {auto i_ok: LT i n} ->
               {auto j_ok: LT j m} ->
               Vertex n m
createVertex n m i j {i_ok} {j_ok} = MkVertex (myNatToFin i n i_ok) (myNatToFin j m j_ok)


getIthNeighbour : Vertex n m -> (i : Fin 4) -> Maybe (Vertex n m)
getIthNeighbour {n} {m} (MkVertex x_fin y_fin) i = let (dx, dy) = index i shift_vec in
      let new_x = x + dx in
      let new_y = y + dy in (case new_x < 0 || new_y < 0 of
           False => (let nat_new_x = the Nat $ cast new_x in
                     let nat_new_y = the Nat $ cast new_y in (case isLTE (S nat_new_x) n of
                                 (Yes prf_x) => (case isLTE (S nat_new_y) m of
                                                    (Yes prf_y) => Just $ createVertex n m _ _ {i_ok=prf_x} {j_ok=prf_y}
                                                    (No contra) => Nothing)
                                 (No contra) => Nothing))
           True => Nothing)
    where x : Integer
          x = the Integer $ cast $ finToNat x_fin
          y : Integer
          y = the Integer $ cast $ finToNat y_fin
                                               


neighbours : Vertex n m -> List (Vertex n m)
neighbours v = catMaybes [ getIthNeighbour v i | i <- [FZ, FS FZ, FS $ FS FZ, FS $ FS $ FS FZ] ]


genEdgesFromVertex : Vertex n m -> List (GridEdge n m)
genEdgesFromVertex v = [ MkEdge v u | u <- neighbours v]


generateGrid : (n : Nat) -> (m : Nat) -> Effects.SimpleEff.Eff (MazeGrid n m) [RND, STDIO] 
generateGrid n m = do
  ?qqq
    {-
    edgeList <- genEdges [initial] $ genEdgesFromVertex n m initial
    pure $ Grid n m edgeList
  where genEdges : List Vertex -> EdgeList -> Effects.SimpleEff.Eff (EdgeList) [RND, STDIO] 
        genEdges _ [] = pure []
        genEdges added_vertices edges_orig@(_ :: _) = do
          edges_rotated <- rotateRandom edges_orig
          let e = Prelude.List.head {ok=believe_me True} edges_rotated
          let edges = tail {ok=believe_me True} edges_rotated
          let to = (snd e)
          let inv_e = (snd e, fst e)
          let new_added_vertices = (to :: added_vertices)
          let edges_from_to = genEdgesFromVertex n m to
          let added_edges = filter (\el => not $ elem (snd el) added_vertices) edges_from_to
          let new_edges = added_edges ++ (filter (\el => (snd el) /= to) edges)
          pure (inv_e :: e :: !(genEdges new_added_vertices new_edges))
          -}


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
  let n_int = the Integer (cast n_str)
  let m_int = the Integer (cast m_str)
  if n_int <= 0 || m_int <= 0
     then raise "Invalid maze parameters"
     else pure ()

  let n = the Nat $ cast n_int
  let m = the Nat $ cast m_int

  t <- time
  srand t

  grid <- generateGrid n m 

  pure ()


main : IO ()
main = run generatePrintMaze
