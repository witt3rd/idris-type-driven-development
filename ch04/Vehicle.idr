%default total

data PowerSource = Petrol | Pedal | Electric

data Vehicle : PowerSource -> Type where
  Unicycle : Vehicle Pedal
  Bicycle : Vehicle Pedal
  Motorcycle : (fuel : Nat) -> Vehicle Petrol
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol
  ECar : (charge : Nat) -> Vehicle Electric
  Tram : (charge : Nat) -> Vehicle Electric

wheels : Vehicle _ -> Nat
wheels Unicycle = 1
wheels Bicycle = 2
wheels (Motorcycle _) = 2
wheels (Car _) = 4
wheels (Bus _) = 4
wheels (ECar _) = 2
wheels (Tram _) = 8

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Motorcycle fuel) = Motorcycle 50
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200

recharge : Vehicle Electric -> Vehicle Electric
recharge (ECar charge) = ECar 5000
recharge (Tram charge) = Tram 20000
