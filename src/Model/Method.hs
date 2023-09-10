module Model.Method
    ( Method(..)
    ) where

data Method
    = Lagrange
    | Newton
    | Gauss
    deriving (Eq, Show)
