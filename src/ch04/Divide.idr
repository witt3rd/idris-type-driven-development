data DivResult = DivByZero | Result Double

safeDivide : Double -> Double -> DivResult
safeDivide x y = if y == 0 then DivByZero
                           else Result (x / y)

data MyMaybe a = Nothing | Just a

safeDivide2 : Double -> Double -> MyMaybe Double
safeDivide2 x y = if y == 0 then Nothing
                            else Just (x / y)

