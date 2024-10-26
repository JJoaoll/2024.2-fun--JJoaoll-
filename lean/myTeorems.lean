namespace funcoes

def curry : (α × β → γ) → (α → β → γ)
  | f, a, b => f (a, b)

def uncurry : (α → β → γ) → (α × β → γ)
  | f, (a, b) => f a b

theorem uncurry_curry :
  ∀(f : α × β → γ), ∀(a : α), ∀(b : β),
  uncurry (curry f) (a, b) = f (a, b) :=
by
  intros f a b
  rw [uncurry]
  rw [curry]

theorem curry_uncurry :
  ∀(f : α → β → γ), ∀(a : α), ∀(b : β),
  curry (uncurry f) a b = f a b :=
by
  intros f a b
  rw [curry]
  rw [uncurry]

def comp : (β → γ) → (α → β) → (α → γ)
  | g, f, a => g (f a)
infixr:80  "o" => comp

theorem comp_ass :
  ∀(f : α → β), ∀(g : β → γ), ∀(h : γ → δ),
  ∀(a : α), ((h o g) o f) a = (h o (g o f)) a :=
by
  intros f g h a
  rw [comp]
  rw [comp]
  rw [comp]
  rw [comp]
