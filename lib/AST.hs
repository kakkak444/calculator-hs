module AST
    ( Ast(..)
    , Literal(..)
    ) where

import AST.Types

data Ast
    = Lit Literal
    | Add Ast Ast | Sub Ast Ast | Mul Ast Ast | Div Ast Ast
    deriving (Show)
