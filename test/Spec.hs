{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import CrispSpec (crispSpec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec do
  crispSpec
