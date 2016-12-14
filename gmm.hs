{-# LANGUAGE DataKinds, NegativeLiterals #-}
module Main where

import           Prelude                          hiding (product)
import           Language.Hakaru.Runtime.Prelude
import           Language.Hakaru.Types.Sing
import qualified System.Random.MWC                as MWC
import           Control.Monad

prog = 
  let_ (lam $ \ as1 ->
        (plate (unsafeNat (nat2int (size as1) +
                           negate (nat2int (nat_ 1)))) $
               \ i3 ->
               beta (summate (i3 + nat_ 1) (size as1) (\ j4 -> as1 ! j4))
                    (as1 ! i3)) >>= \ xs2 ->
        dirac (array (size as1) $
                     \ i5 ->
                     let_ (product (nat_ 0) i5 (\ j7 -> xs2 ! j7)) $ \ x6 ->
                     x6 *
                     case_ (i5 + nat_ 1 == size as1)
                           [branch ptrue (nat2prob (nat_ 1)),
                            branch pfalse
                                   (unsafeProb (nat2real (nat_ 1) +
                                                negate (fromProb (xs2 ! i5))))])) $ \ dirichlet0 ->
  let_ (array (nat_ 3) $
              \ x9 ->
              case_ (x9 == nat_ 0)
                    [branch ptrue (prob_ 1),
                     branch pfalse
                            (case_ (x9 == nat_ 1)
                                   [branch ptrue (prob_ 1), branch pfalse (prob_ 1)])]) $ \ as8 ->
  let_ (nat_ 300) $ \ data_size10 ->
  dirichlet0 `app` as8 >>= \ theta11 ->
  let_ (array (nat_ 3) $
              \ x13 ->
              case_ (x13 == nat_ 0)
                    [branch ptrue (negate (nat2int (nat_ 7))),
                     branch pfalse
                            (nat2int (case_ (x13 == nat_ 1)
                                            [branch ptrue (nat_ 3),
                                             branch pfalse (nat_ 10)]))]) $ \ phi12 ->
  (plate data_size10 $
         \ i14 ->
         categorical theta11 >>= \ z15 ->
         normal (fromInt (phi12 ! z15)) (nat2prob (nat_ 1)))

main :: IO ()
main = do
  g <- MWC.createSystemRandom
  forever $ run g prog
