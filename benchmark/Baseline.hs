module Baseline where

baseProgram :: Int -> (Int, Int)
baseProgram n = if n <= 0
                then (n, n)
                else baseProgram (n - 1)
