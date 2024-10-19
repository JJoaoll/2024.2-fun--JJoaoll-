
module Test where

factorial : ℕ → ℕ
factorial 0 = 1
factorial (suc n) = suc n * factorial n
