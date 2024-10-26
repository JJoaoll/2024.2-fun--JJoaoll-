inductive Weekday where
  | sunday    : Weekday
  | monday    : Weekday
  | tuesday   : Weekday
  | wednesday : Weekday
  | thursday  : Weekday
  | friday    : Weekday
  | saturday  : Weekday

#check Weekday.sunday
#check Weekday.monday

open Weekday

#check sunday
#check monday

inductive MyNat where
| Zero : MyNat
| Succ : MyNat → MyNat
deriving Repr

open MyNat

#check Zero
#check Succ Zero
#check Succ (Succ Zero)

def add (n m : MyNat) : MyNat :=
  match n, m with
  | n, Zero => n
  | n, Succ m => Succ (add n m)

#eval add Zero Zero
#eval add (Succ ( Succ Zero)) (Succ (Succ ( Succ Zero)))
