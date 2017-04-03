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


data Vertex : Nat -> Nat -> Type where
  MkVertex : (i : Fin n) -> (j : Fin m) -> Vertex n m


data GenericEdge : Type -> Type where
  MkEdge : a -> a -> GenericEdge a


GridEdge : Nat -> Nat -> Type
GridEdge n m = GenericEdge (Vertex n m)


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
 

vertexToFin : Vertex n m -> Fin (m * n + m)
vertexToFin {n} {m} (MkVertex i j) = shiftByFin j $ mulFin i m
