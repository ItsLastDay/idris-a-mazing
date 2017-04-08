module Main

import Data.Vect
import Data.Fin

import Effects
import Effect.Exception
import Effect.StdIO
import Effect.System
import Effect.State
import Effect.Random


shiftVec : Vect 4 (Integer, Integer)
shiftVec = [(-1, 0), (0, 1), (0, -1), (1, 0)]


data Vertex : Nat -> Nat -> Type where
  MkVertex : (i : Fin n) -> (j : Fin m) -> Vertex n m


implementation Eq (Vertex n m) where
  (==) (MkVertex i j) (MkVertex q w) = i == q && j == w


data GenericEdge : Type -> Type where
  MkEdge : a -> a -> GenericEdge a


GridEdge : Nat -> Nat -> Type
GridEdge n m = GenericEdge (Vertex n m)

implementation Eq a => Eq (GenericEdge a) where
  (==) (MkEdge x y) (MkEdge u v) = x == u && y == v

implementation Show (Vertex n m) where
  show (MkVertex i j) = "[" ++ (show $ finToNat i) ++ ", " ++ (show $ finToNat j) ++ "]"

implementation Show a => Show (GenericEdge a) where
  show (MkEdge x y) = "(" ++ show x ++ ", " ++ show y ++ ")"


||| An n x m Maze is a graph. 
||| It has n * m vertices. 
||| Numeration goes from 0.
||| 
||| Example of 1x2 maze (along with indices of rows\columns):
|||       0 1 
|||         _ 
|||  0   |. .|
|||       _
|||
data MazeGrid : (n : Nat) -> (m : Nat) -> Type where
  Grid : (n : Nat) -> (m : Nat) -> (edges : List (GridEdge n m)) -> MazeGrid n m


DistVector : Nat -> Nat -> Type
DistVector n m = Vect (m * n + m) Integer


getEdges : MazeGrid n m -> List (GridEdge n m)
getEdges (Grid n m edges) = edges


isLT : (m : Nat) -> (n : Nat) -> Dec (LT m n)
isLT m n = isLTE (S m) n

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


whereEdgeStarts : GridEdge n m -> Vertex n m
whereEdgeStarts (MkEdge u v) = u


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
getIthNeighbour {n} {m} (MkVertex xFin yFin) i = 
    let (dx, dy) = index i shiftVec in
    let newX = x + dx in
    let newY = y + dy in (case newX < 0 || newY < 0 of
         False => (let natNewX = the Nat $ cast newX in
                   let natNewY = the Nat $ cast newY in (case isLT natNewX n of
                               (Yes prfX) => (case isLT natNewY m of
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


neighboursInGrid : Vertex n m -> MazeGrid n m -> List (Vertex n m)
neighboursInGrid x (Grid n m edges) = map whereEdgePoints $ filter (\edge => (whereEdgeStarts edge) == x) edges


genEdgesFromVertex : Vertex n m -> (p : Nat ** (Vect p (GridEdge n m)))
genEdgesFromVertex v = (_ ** fromList [ MkEdge v u | u <- neighbours v])


edgeNotPontsToAnyVertex : GridEdge n m -> List (Vertex n m) -> Bool
edgeNotPontsToAnyVertex (MkEdge x y) xs = not $ Prelude.List.elem y xs


-- already added vertices -> list of unprocessed edges -> list of resulting edges
genEdges : List (Vertex n m) -> Vect len (GridEdge n m) -> Eff (List (GridEdge n m)) [RND, STDIO] 
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


generateGrid : (n : Nat) -> (m : Nat) -> LT 0 n -> LT 0 m -> Eff (MazeGrid n m) [RND, STDIO] 
generateGrid n m prfN prfM = do
    edgeList <- genEdges [initialVertex] $ snd $ genEdgesFromVertex initialVertex
    pure $ Grid n m edgeList
  where initialVertex : Vertex n m
        initialVertex = createVertex n m 0 0 {iOk=prfN} {jOk=prfM}


bfs : MazeGrid n m -> List (Vertex n m) -> Effects.SimpleEff.Eff (DistVector n m) [STATE (DistVector n m)]
bfs _ [] = get
bfs gr@(Grid n m edges) (x :: xs) = do
    curDist <- get
    let unvisitedNeighbours = filter (\vert => (index (vertexToFin vert) curDist) == -1) $ neighboursInGrid x gr 
    update $ updUnvisited ((index (vertexToFin x) curDist) + 1) unvisitedNeighbours
    bfs gr (xs ++ unvisitedNeighbours)
  where updUnvisited : Integer -> List (Vertex n m) -> DistVector n m -> DistVector n m
        updUnvisited x [] y = y
        updUnvisited x (z :: xs) y = updUnvisited x xs (updateAt (vertexToFin z) (const x) y)

getDistFrom : MazeGrid n m -> Vertex n m -> DistVector n m
getDistFrom {n} {m} grid v = runPureInit [genInitialList] $ bfs grid [v] 
    where genInitialList : DistVector n m
          genInitialList = updateAt (vertexToFin v) (const 0) $ replicate (m * n + m) (-1)



parseDimension : Integer -> Eff ((n : Nat ** LT 0 n)) [EXCEPTION String]
parseDimension x = case isLT 0 xNat of
                        (Yes prf) => pure (xNat ** prf)
                        (No contra) => raise ("Invalid maze dimension " ++ show x)
    where xNat : Nat
          xNat = the Nat $ cast x


printMaze : MazeGrid n m -> Vertex n m -> Vertex n m -> DistVector n m -> DistVector n m -> Integer -> Eff () [STDIO]
printMaze (Grid n m edges) start end distFromStart distFromEnd distStartToEnd = printRow 0
    where printRow : Nat -> Eff () [STDIO]
          printRow rowIdx = 
              case isLT rowIdx n of
                  (Yes prfRow) => do
                      printColAtRow 0 rowIdx prfRow
                      printColBetweenRows 0 rowIdx prfRow
                      printRow (rowIdx + 1)
                  (No contra) => putStrLn "End"
            where printColBetweenRows : Nat -> (row : Nat) -> LT row n -> Eff () [STDIO]
                  printColBetweenRows colIdx rowIdx prfRow = 
                      case isLT (S rowIdx) n of
                          (Yes prfRowNext) => (case isLT colIdx m of
                                                (Yes prfCol) => do
                                                  let curVert = createVertex n m rowIdx colIdx {iOk=prfRow} {jOk=prfCol}
                                                  let nextVert = createVertex n m (S rowIdx) colIdx {iOk=prfRowNext} {jOk=prfCol}
                                                  putChar (if elem (MkEdge curVert nextVert) edges then ' ' else '_')
                                                  putChar '|'
                                                  printColBetweenRows (colIdx + 1) rowIdx prfRow
                                                (No contra) => putChar '\n')
                          (No contra) => pure ()

                  printColAtRow : Nat -> (row : Nat) -> LT row n -> Eff () [STDIO]
                  printColAtRow colIdx rowIdx prfRow = 
                      case isLT colIdx m of
                           (Yes prfCol) => do
                             putChar (decideCellCharacter prfCol)
                             let curVertex = createVertex n m rowIdx colIdx {iOk=prfRow} {jOk=prfCol} 
                             case isLT (S colIdx) m  of 
                                  (Yes prfNextCol) => do
                                    let nextVertex = createVertex n m rowIdx (S colIdx) {iOk=prfRow} {jOk=prfNextCol}
                                    putChar (if elem (MkEdge curVertex nextVertex) edges then ' ' else '|')
                                  (No contra) => pure ()
                             printColAtRow (S colIdx) rowIdx prfRow
                           (No contra) => putChar '\n'
                      where decideCellCharacter : LT colIdx m -> Char
                            decideCellCharacter prfCol = 
                              let curVertex = createVertex n m rowIdx colIdx {iOk=prfRow} {jOk=prfCol} in
                              let distToStart = index (vertexToFin curVertex) distFromStart in
                              let distToEnd = index (vertexToFin curVertex) distFromEnd
                              in 
                                  if curVertex == start then 'S'
                                  else if curVertex == end then 'E'
                                  else
                                    if distStartToEnd == distToStart + distToEnd 
                                    then 'o'
                                    else '.'


genProvableBoundedNat : (m : Nat) -> Eff ((n : Nat ** LT n m)) [RND, EXCEPTION String]
genProvableBoundedNat m = let nNat = the Nat $ cast $ !(rndInt 0 ((the Integer $ cast m) - 1)) in
                              case isLTE (S nNat) m of
                               (Yes prf) => pure (nNat ** prf)
                               (No contra) => raise "Number generated by rndInt was not in bounds"


lteMinusPrf : (n : Nat) -> LT 0 n -> LT (minus n 1) n
lteMinusPrf n nPrf with (nPrf)
  lteMinusPrf (S right) nPrf | (LTESucc x) = rewrite minusZeroRight right in lteRefl


-- debug output
printAligned : Show a => Nat -> Nat -> List a -> Eff () [STDIO]
printAligned n m [] = putChar '\n'
printAligned n m (x :: xs) = do
  if (((Prelude.List.length xs) + 1) `mod` m) == 0
     then putChar '\n'
     else pure ()
  putStr $ show x
  putChar ' '
  printAligned n m xs



generatePrintMaze : Eff () [STDIO, SYSTEM, RND, EXCEPTION String]
generatePrintMaze = do
  [prog, nStr, mStr] <- getArgs | [] => putStrLn "Can't happen"
                          | [prog] => putStrLn "No arguments"
                          | [prog, n] => putStrLn "Not enough arguments"
                          | _ => putStrLn "Too many arguments" 
  
  (n ** nPrf) <- parseDimension (the Integer $ cast nStr)
  (m ** mPrf) <- parseDimension (the Integer $ cast mStr)

  t <- time
  srand t

  grid <- generateGrid n m nPrf mPrf

  (enterColumn ** enterColumnPrf) <- genProvableBoundedNat m 
  let startVertex = createVertex n m 0 enterColumn {jOk=enterColumnPrf} 
  (exitColumn ** exitColumnPrf) <- genProvableBoundedNat m
  let endVertex = createVertex n m (n - 1) exitColumn {jOk=exitColumnPrf} {iOk=lteMinusPrf n nPrf}

  let distFromStart = getDistFrom grid startVertex 
  let distFromEnd = getDistFrom grid endVertex
  let distStartToEnd = index (vertexToFin endVertex) distFromStart
  
  printMaze grid startVertex endVertex distFromStart distFromEnd distStartToEnd
  pure ()


main : IO ()
main = run generatePrintMaze
