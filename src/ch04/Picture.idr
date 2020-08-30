module Picture

import Shape

%default total

public export
data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

%name Picture pic, pic1, pic2

rect : Picture
rect = Primitive (Rectangle 20 10)

circ : Picture
circ = Primitive (Circle 5)

tri : Picture
tri = Primitive (Triangle 10 10)

tri2 : Picture
tri2 = Primitive (Triangle 20 20)

testPicture : Picture
testPicture = Combine (Translate 5 5 rect) 
              (Combine (Translate 25 5 circ)
              (Combine (Translate 15 25 tri)
              (Translate 10 40 tri2)))

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic1 pic2) = (pictureArea pic1) + (pictureArea pic2)
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic

data Biggest = NoTriangle | Size Double

compareTriangle : Biggest -> Biggest -> Biggest
compareTriangle NoTriangle NoTriangle = NoTriangle
compareTriangle NoTriangle (Size x) = Size x
compareTriangle (Size x) NoTriangle = Size x
compareTriangle (Size x) (Size y) = if x > y then Size x else Size y

biggestTriangle : Picture -> Biggest
biggestTriangle (Primitive (Triangle x y)) = Size (area (Triangle x y))
biggestTriangle (Primitive _) = NoTriangle
biggestTriangle (Combine pic1 pic2) = let t1 = biggestTriangle pic1
                                          t2 = biggestTriangle pic2 in
                                          compareTriangle t1 t2
biggestTriangle (Rotate _ pic) = biggestTriangle pic
biggestTriangle (Translate _ _ pic) = biggestTriangle pic
