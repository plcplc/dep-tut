-- doesn't work on my current setup: {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
-- so we roll by hand instead
--
module Main where

import Spec (spec)
import Test.Hspec

main :: IO ()
main = hspec spec
