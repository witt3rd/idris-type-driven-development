import Tree
import Shape
import Picture

%default total

listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

treeToList : Ord a => Tree a -> List a
treeToList Empty = []
treeToList (Node left val right) = (treeToList left) ++ (val :: (treeToList right))

data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr

eval : Expr -> Int
eval (Val x) = x
eval (Add x y) = (eval x) + (eval y)
eval (Sub x y) = (eval x) - (eval y)
eval (Mul x y) = (eval x) * (eval y)

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing y = y
maxMaybe (Just x) Nothing = Just x
maxMaybe (Just x) (Just y) = case (compare x y) of
                                  LT => Just y
                                  EQ => Just x
                                  GT => Just x

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive x@(Triangle _ _)) = Just (area x)
biggestTriangle (Primitive _) = Nothing
biggestTriangle (Combine pic1 pic2) = let bt1 = biggestTriangle pic1
                                          bt2 = biggestTriangle pic2 in
                                          maxMaybe bt1 bt2
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic

testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                   (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                   (Primitive (Circle 4))
