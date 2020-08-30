import Data.Vect

%default total

createEmpties : Num a => {n:_} -> Vect n (Vect 0 a)
createEmpties = replicate _ []

transposeMat : Num a => {n:_} -> Vect m (Vect n a) -> Vect n (Vect m a)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsT = transposeMat xs in
                             zipWith (::) x xsT

addMat : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMat [] [] = []
addMat (x :: xs) (y :: ys) = zipWith (+) x y :: addMat xs ys

dot : Num a => Vect n a -> Vect n a -> a
dot [] [] = 0
dot xs ys = sum $ zipWith (*) xs ys

dotVec : Num a => Vect n a -> Vect m (Vect n a) -> Vect m a
dotVec xs [] = []
dotVec xs (y :: ys) = dot xs y :: dotVec xs ys

matMulAux : Num a => Vect n (Vect m a) -> Vect p (Vect m a) -> Vect n (Vect p a)
matMulAux [] ys = []
matMulAux (x :: xs) ys = dotVec x ys :: matMulAux xs ys

matMul : Num a => {p:_} -> Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
matMul [] [] = []
matMul xs ys = matMulAux xs (transposeMat ys)
