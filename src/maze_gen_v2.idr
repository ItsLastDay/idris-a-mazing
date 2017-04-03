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


implementation Eq (Vertex n m) where
  (==) (MkVertex i j) (MkVertex q w) = i == q && j == w


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


reversedEdge : GridEdge n m -> GridEdge n m
reversedEdge (MkEdge u v) = MkEdge v u


whereEdgePoints : GridEdge n m -> Vertex n m
whereEdgePoints (MkEdge u v) = v


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
               {auto iOk: LT i n} ->
               {auto jOk: LT j m} ->
               Vertex n m
createVertex n m i j {iOk} {jOk} = MkVertex (myNatToFin i n iOk) (myNatToFin j m jOk)


getIthNeighbour : Vertex n m -> (i : Fin 4) -> Maybe (Vertex n m)
getIthNeighbour {n} {m} (MkVertex xFin yFin) i = let (dx, dy) = index i shift_vec in
      let newX = x + dx in
      let newY = y + dy in (case newX < 0 || newY < 0 of
           False => (let natNewX = the Nat $ cast newX in
                     let natNewY = the Nat $ cast newY in (case isLTE (S natNewX) n of
                                 (Yes prfX) => (case isLTE (S natNewY) m of
                                                    (Yes prfY) => Just $ createVertex n m _ _ {iOk=prfX} {jOk=prfY}
                                                    (No contra) => Nothing)
                                 (No contra) => Nothing))
           True => Nothing)
    where x : Integer
          x = the Integer $ cast $ finToNat xFin
          y : Integer
          y = the Integer $ cast $ finToNat yFin
                                               


neighbours : Vertex n m -> List (Vertex n m)
neighbours v = catMaybes [ getIthNeighbour v i | i <- [FZ, FS FZ, FS $ FS FZ, FS $ FS $ FS FZ] ]


genEdgesFromVertex : Vertex n m -> (p : Nat ** (Vect p (GridEdge n m)))
genEdgesFromVertex v = (_ ** fromList [ MkEdge v u | u <- neighbours v])


edgeNotPontsToAnyVertex : GridEdge n m -> List (Vertex n m) -> Bool
edgeNotPontsToAnyVertex (MkEdge x y) xs = not $ Prelude.List.elem y xs


-- already added vertices -> list of unprocessed edges -> list of resulting edges
genEdges : List (Vertex n m) -> Vect len (GridEdge n m) -> Effects.SimpleEff.Eff (List (GridEdge n m)) [RND, STDIO] 
genEdges _ [] = pure []
genEdges {len=(S r)} addedVertices edgesPool = do
    edgesRotated <- rotateRandom edgesPool
    let (chosenEdge :: remainingPool) = edgesRotated
    pure (chosenEdge :: (reversedEdge chosenEdge) :: 
      !(genEdges (calcNewVertices addedVertices chosenEdge) (snd $ calcNewEdgesPool remainingPool chosenEdge addedVertices)))

  where calcNewVertices : List (Vertex n m) -> GridEdge n m -> List (Vertex n m)
        calcNewVertices xs (MkEdge x y) = y :: xs

        calcNewEdgesPool : Vect len1 (GridEdge n m) -> GridEdge n m -> List (Vertex n m) -> (p : Nat ** Vect p (GridEdge n m))
        calcNewEdgesPool pool (MkEdge fr to) vertices = 
          let newEdges = snd $ filter (\edge => edgeNotPontsToAnyVertex edge vertices) (snd $ genEdgesFromVertex to) in 
          let filteredOldEdges = snd $ filter (\edge => (whereEdgePoints edge) /= to) pool in
              (_ ** (newEdges ++ filteredOldEdges))


generateGrid : (n : Nat) -> (m : Nat) -> LT 0 n -> LT 0 m -> Effects.SimpleEff.Eff (MazeGrid n m) [RND, STDIO] 
generateGrid n m prf_n prf_m = do
    edgeList <- genEdges [initialVertex] $ snd $ genEdgesFromVertex initialVertex
    pure $ Grid n m edgeList
  where initialVertex : Vertex n m
        initialVertex = createVertex n m 0 0 {iOk=prf_n} {jOk=prf_m}


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

  -- maybe do smth like "(S k) <- n | blabla" to get rid of believe_me?
  grid <- generateGrid n m (believe_me True) (believe_me True)

  pure ()


main : IO ()
main = run generatePrintMaze
