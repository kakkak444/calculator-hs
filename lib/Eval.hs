{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Eval
    ( eval
    ) where

import AST
import AST.Types

eval :: Ast -> Literal
{-# INLINE eval #-}
eval (Lit lit) = lit
eval (Add l r) = applyToNum2 (+) (eval l) (eval r)
eval (Sub l r) = applyToNum2 (-) (eval l) (eval r)
eval (Mul l r) = applyToNum2 (*) (eval l) (eval r)
eval (Div l r) = applyToNum2 (/) (eval l) (eval r)


applyToNum2 :: (SomeNumber -> SomeNumber -> SomeNumber) -> Literal -> Literal -> Literal
{-# INLINE applyToNum2 #-}
applyToNum2 op = \(Num l) (Num r) -> Num $ l `op` r
