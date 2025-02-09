module AST.Types
    ( Literal(..)
    , SomeNumber(..)
    , Number(..)
    , NumType(..)
    ) where

import AST.Types.Number

data Literal
    = Num SomeNumber
    deriving (Show)
