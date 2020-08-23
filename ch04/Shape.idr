module Shape

%default total

||| Represents various shapes
public export
data Shape = ||| A triangle, with its base and length and height
             Triangle Double Double
           | ||| A rectangle, with its length and height
             Rectangle Double Double
           | ||| A circle, with its radius
             Circle Double

export
area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius
