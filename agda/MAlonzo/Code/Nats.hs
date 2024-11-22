{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module MAlonzo.Code.Nats where

import MAlonzo.RTE (coe, erased, AgdaAny, addInt, subInt, mulInt,
                    quotInt, remInt, geqInt, ltInt, eqInt, add64, sub64, mul64, quot64,
                    rem64, lt64, eq64, word64FromNat, word64ToNat)
import qualified MAlonzo.RTE
import qualified Data.Text

-- Nats.ℕ
d_ℕ_2 = ()
data T_ℕ_2 = C_zero_4 | C_succ_6 T_ℕ_2
-- Nats.pred
d_pred_8 :: T_ℕ_2 -> T_ℕ_2
d_pred_8 v0
  = case coe v0 of
      C_zero_4 -> coe v0
      C_succ_6 v1 -> coe v1
      _ -> MAlonzo.RTE.mazUnreachableError
