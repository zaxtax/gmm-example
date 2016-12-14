{-# LANGUAGE DataKinds, NegativeLiterals #-}
module Main where

import           Prelude                          hiding (product)
import           Language.Hakaru.Runtime.Prelude
import           Language.Hakaru.Types.Sing
import qualified System.Random.MWC                as MWC
import           Control.Monad

prog = 
  (plate (nat_ 10000) $
         \ i0 ->
         categorical (array (nat_ 3) $
                            \ x1 ->
                            case_ (x1 == nat_ 0)
                                  [branch ptrue (nat2prob (nat_ 1)),
                                   branch pfalse
                                          (case_ (x1 == nat_ 1)
                                                 [branch ptrue (nat2prob (nat_ 1)),
                                                  branch pfalse (nat2prob (nat_ 1))])]))

main :: IO ()
main = do
  g <- MWC.createSystemRandom
  forever $ run g prog
