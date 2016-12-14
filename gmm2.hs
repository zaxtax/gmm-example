{-# LANGUAGE DataKinds, NegativeLiterals #-}
module Main where

import           Prelude                          hiding (product)
import           Language.Hakaru.Runtime.Prelude
import           Language.Hakaru.Types.Sing
import qualified System.Random.MWC                as MWC
import           Control.Monad

prog = 
  lam $ \ as0 ->
  lam $ \ z1 ->
  lam $ \ t2 ->
  lam $ \ docUpdate3 ->
  categorical (array (size as0) $
                     \ zNew64 ->
                     exp (summate (nat_ 0)
                                  (size as0)
                                  (\ _a9105 ->
                                   summate (nat_ 0)
                                           (size t2)
                                           (\ i6 ->
                                            case_ (_a9105
                                                   == case_ (i6 == docUpdate3)
                                                            [branch ptrue (zNew64),
                                                             branch pfalse (z1 ! i6)])
                                                  [branch ptrue (t2 ! i6),
                                                   branch pfalse (nat2real (nat_ 0))]
                                            ^ nat_ 2) *
                                   fromProb (recip (nat2prob (nat_ 1 +
                                                              summate (nat_ 0)
                                                                      (size t2)
                                                                      (\ i7 ->
                                                                       case_ (_a9105
                                                                              == case_ (i7
                                                                                        == docUpdate3)
                                                                                       [branch ptrue
                                                                                               (zNew64),
                                                                                        branch pfalse
                                                                                               (z1
                                                                                                ! i7)])
                                                                             [branch ptrue (nat_ 1),
                                                                              branch pfalse
                                                                                     (nat_ 0)])))) *
                                   fromProb (recip (nat2prob (nat_ 2))))) *
                     recip (nat_ 2
                            `thRootOf` (nat2prob (product (nat_ 0)
                                                          (size as0)
                                                          (\ _a8 ->
                                                           nat_ 2 +
                                                           summate (nat_ 0)
                                                                   (size t2)
                                                                   (\ _a9109 ->
                                                                    case_ (_a8
                                                                           == case_ (_a9109
                                                                                     == docUpdate3)
                                                                                    [branch ptrue
                                                                                            (zNew64),
                                                                                     branch pfalse
                                                                                            (z1
                                                                                             ! _a9109)])
                                                                          [branch ptrue (nat_ 1),
                                                                           branch pfalse (nat_ 0)] *
                                                                    nat_ 2))))) *
                     (nat2prob (summate (nat_ 0)
                                        (size t2)
                                        (\ _a91010 ->
                                         case_ (_a91010 == docUpdate3)
                                               [branch ptrue (nat_ 0),
                                                branch pfalse
                                                       (case_ (zNew64 == z1 ! _a91010)
                                                              [branch ptrue (nat_ 1),
                                                               branch pfalse (nat_ 0)])])) +
                      as0 ! zNew64) *
                     recip (nat2prob (summate (nat_ 0)
                                              (size t2)
                                              (\ _a91011 ->
                                               case_ (_a91011 == docUpdate3)
                                                     [branch ptrue (nat_ 0),
                                                      branch pfalse
                                                             (case_ (z1 ! _a91011 < nat_ 0)
                                                                    [branch ptrue (nat_ 0),
                                                                     branch pfalse (nat_ 1)])])) +
                            summate (nat_ 0) (size as0) (\ _a91012 -> as0 ! _a91012)))

main :: IO ()
main = do
  g <- MWC.createSystemRandom
  print prog
