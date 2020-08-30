data Shape = ||| A triangle, with its base and length and height
             Triangle Double Double
           | ||| A rectangle, with its length and height
             Rectangle Double Double
           | ||| A circle, with its radius
             Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius
             
Eq Shape where
  (==) (Triangle x y)   (Triangle x' y')  = x == x' && y == y' 
  (==) (Rectangle x y)  (Rectangle x' y') = x == x' && y == y'
  (==) (Circle x)       (Circle x')       = x == x'
  (==) _                _                 = False

Ord Shape where
  compare x y = compare (area x) (area y)

testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4, Rectangle 2 7]

