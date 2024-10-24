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

open MyNat

#check Zero  
#check Succ Zero
#check Succ (Succ Zero)


