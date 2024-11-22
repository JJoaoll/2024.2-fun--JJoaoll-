namespace MyList

--import List as L

inductive Nat : Type
  | O : Nat
  | S : Nat → Nat

open Nat

inductive List (α : Type) : Type
  | Nil  : List α
  | Cons : α → List α → List α
  deriving Repr, BEq

open List

def take {α : Type} : Nat → List α → List α
  | _, Nil              => Nil
  | O, _                => Nil
  | S n, Cons x xs => Cons x (take n xs)

def drop {α : Type} : Nat → List α → List α
  | _, Nil           => Nil
  | O, xs            => xs
  | S n, Cons _ xs => drop n xs

#check id

def concat : List α → List α → List α
  | Nil, ys       => ys
  | Cons x xs, ys => Cons x (concat xs ys)

#eval concat (Cons 3 Nil) (Cons 1 (Cons 2 Nil))


def comp : (β → γ) → (α → β) → (α → γ)
  | g, f, x => g (f x)

infixl: 55 " ∘ "   => comp


theorem concat_take_drop {α : Type} : ∀(xs : List α), ∀(n : Nat),
  concat (take n xs) (drop n xs) = xs :=
by
  intro xs
  induction xs with
  | Nil =>
    intro n
    rw [take]
    rw [drop]
    rw [concat]

  | Cons k ks HI =>
      intro n
      match n with
      | O =>
        rw [take]
        rw [drop]
        rw [concat]
        exact List.noConfusion
        exact List.noConfusion

      |S k =>
        rw [take]
        rw [drop]
        rw [concat]
        rw [HI k]

theorem take_drop_drop_take {α : Type} : ∀(xs: List α), ∀(n m : Nat),
  (comp (take m) (drop n)) xs = (comp (drop m) (take n)) xs :=
by
  intro xs
  induction xs with
    | Nil =>
      intros n m
      rw [comp]; rw [comp];
      rw [take]; rw [drop];
      rw [take]; rw [drop];

    | Cons k ks HI =>
      intros n m
      match n, m with
      | O, O         =>
        simp[comp, take, drop]

      | (S n'), O     =>
        simp[comp, drop]
        simp[comp] at HI
        rw [take]
        sorry
        



      | O, (S q)     => sorry
      | (S k), (S q) => sorry

