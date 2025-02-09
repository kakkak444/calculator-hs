module Utils
    ( (|>)
    ) where

infixl 0 |>

-- | flip version of ($)
(|>) :: a -> (a -> b) -> b
a |> f = f a