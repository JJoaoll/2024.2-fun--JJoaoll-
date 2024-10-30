module Funcoes where

open import Data.Product using (_×_; _,_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

-- Definição de curry e uncurry
curry : (α × β → γ) → (α → β → γ)
curry f a b = f (a , b)

uncurry : (α → β → γ) → (α × β → γ)
uncurry f (a , b) = f a b

-- Teorema uncurry_curry
uncurry-curry : (f : α × β → γ) → (a : α) → (b : β) → uncurry (curry f) (a , b) ≡ f (a , b)
uncurry-curry f a b = refl

-- Teorema curry_uncurry
curry-uncurry : (f : α → β → γ) → (a : α) → (b : β) → curry (uncurry f) a b ≡ f a b
curry-uncurry f a b = refl

-- Definição de composição de funções (comp)
comp : (β → γ) → (α → β) → (α → γ)
comp g f a = g (f a)
infixr 80 _∘_ = comp

-- Teorema de associatividade da composição de funções
comp-assoc : (f : α → β) → (g : β → γ) → (h : γ → δ) → (a : α) →
             ((h ∘ g) ∘ f) a ≡ (h ∘ (g ∘ f)) a
comp-assoc f g h a = refl
