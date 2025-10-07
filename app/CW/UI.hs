{-# LANGUAGE OverloadedStrings #-}

module CW.UI where

data Pt = Pt Double Double
    deriving (Show, Eq)

data Input = Quit | Mouse Pt
    deriving (Show, Eq)
