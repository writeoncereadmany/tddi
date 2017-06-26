data Powersource = Petrol | Electric | Pedal

data Vehicle : Powersource -> Type where
  Unicycle : Vehicle Pedal
  Bicycle : Vehicle Pedal
  Motorcycle : (fuel : Nat) -> Vehicle Petrol
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol
  Tram : Vehicle Electric

total
wheels : Vehicle _ -> Nat
wheels Unicycle = 1
wheels Bicycle = 2
wheels (Motorcycle _)= 2
wheels (Car _) = 4
wheels (Bus _) = 4
wheels Tram = 12

total
refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Motorcycle fuel) = Motorcycle 50
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
