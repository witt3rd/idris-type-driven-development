import ch08.EqNat
import ch08.CheckEqMaybe

data Vect : Nat -> Type -> Type where
  Nil  : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

{-
-- Attempt #1: while we can compare m and len for equality, the return
-- type can't be satisfied, since Idris can't unify len and m in the types.
exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m} len input =
     case m == len of
          False  => Nothing
          True   => Just input
-}

exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m} len input = 
  case checkEqNat m len of
    Nothing         => Nothing
    Just (Same len) => Just input

exactLength' : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength' {m} len input = 
  case checkEqNat' m len of
    Nothing   => Nothing
    Just Refl => Just input
