structure Integer where
  Z        : Type u
  zero     : Z
  one      : Z
  plus     : Z -> Z -> Z
  minus    : Z -> Z
  times    : Z -> Z -> Z

  ZA_Ass   (a b c : Z) : plus (plus a b) c = plus a (plus b c)
  ZA_IdR   (a : Z)     : plus a zero = a
  ZA_InvR  (a : Z)     : plus a (minus a) = zero
  ZA_Com   (a b : Z)   : plus a b = plus b a

  ZM_Ass   (a b c : Z) : times (times a b) c = times a (times b c)
  ZM_IdR   (a : Z)     : times a one = a
  ZM_Com   (a b : Z)   : times a b = times b a

  Z_DistR (d a b : Z) : times (plus a b) d = times (plus a d) (plus b d)
  -- V2
  Z_NZD   (a b : Z)   : times a b = zero → a = zero ∨ b = zero
