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


shiftByFin : Fin n -> Fin x -> {auto xn_le: LTE 1 n} -> Fin (n - 1 + x)
{-
shiftByFin {n=Z} FZ y impossible
shiftByFin {n=(S k)} {x} FZ y = weakenN (S k) y
shiftByFin {n=Z} (FS a) b impossible
shiftByFin {n=(S k)} {x} (FS a) b = rewrite sym (plusSuccRightSucc x k) in shift 1 $ shiftByFin a b
-}


mulFin : Fin n -> (m : Nat) -> {auto n_le_mult: LTE m (mult m n)} -> Fin (m * n - m + 1)
{-
mulFin x Z = absurd $ mulFin x Z
mulFin {n} x (S Z) = rewrite plusZeroRightNeutral n in x
mulFin {n=Z} x (S k) impossible
mulFin {n=(S t)} x (S k) = rewrite plusCommutative t (k * (S t)) in 
                                   rewrite plusSuccRightSucc (k * (S t)) t in shiftByFin x (mulFin x k)
                                   -}                                   

vertexToFin : Vertex n m -> Fin (n * m)
vertexToFin {n} {m} (MkVertex i j) = shiftByFin {xn_le=getLTELemma1 _ j} j $ mulFin {n_le_mult=getLTELemma2 n m i} i m
    where getLTELemma1 : (n : Nat) -> Fin n -> LTE 1 n 

          getLTELemma2 : (n : Nat) -> (m : Nat) -> Fin n -> LTE m (mult m n)




