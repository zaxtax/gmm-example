{-# LANGUAGE DataKinds, NegativeLiterals #-}
module GmmGibbs2 where

import           Prelude                          hiding (product)
import           Language.Hakaru.Runtime.Prelude
import           Language.Hakaru.Types.Sing
import qualified System.Random.MWC                as MWC
import           Control.Monad
import           Data.Number.LogFloat hiding (product)
import qualified Data.Vector.Generic              as G


gmmGibbs2 =
  lam $ \ as0 ->
  lam $ \ z1 ->
  lam $ \ t2 ->
  lam $ \ docUpdate3 ->
  categorical (gmmTestArray as0 z1 t2 docUpdate3)

gmmTestArray as0 z1 t2 docUpdate3 =
  array (size as0) $
  \ zNew611 ->
           exp (summate (nat_ 0)
                        (size as0)
                        (\ _a91012 ->
                         summate (nat_ 0)
                                 (size t2)
                                 (\ i13 ->
                                  case_ (_a91012
                                         == case_ (i13
                                                   == docUpdate3)
                                                  [branch ptrue
                                                          (zNew611),
                                                   branch pfalse
                                                          (z1
                                                           ! i13)])
                                        [branch ptrue (t2 ! i13),
                                         branch pfalse
                                                (nat2real (nat_ 0))])
                         ^ nat_ 2 *
                         fromProb (recip (nat2prob (nat_ 1 +
                                                    summate (nat_ 0)
                                                            (size t2)
                                                            (\ i14 ->
                                                             case_ (_a91012
                                                                    == case_ (i14
                                                                              == docUpdate3)
                                                                             [branch ptrue
                                                                                     (zNew611),
                                                                              branch pfalse
                                                                                     (z1
                                                                                      ! i14)])
                                                                   [branch ptrue
                                                                           (nat_ 1),
                                                                    branch pfalse
                                                                           (nat_ 0)]))))) *
                                                fromProb (recip (nat2prob (nat_ 2)))) *
                                           recip (nat_ 2
                                                  `thRootOf` (nat2prob (product (nat_ 0)
                                                                                (size as0)
                                                                                (\ _a15 ->
                                                                                 nat_ 2 +
                                                                                 summate (nat_ 0)
                                                                                         (size t2)
                                                                                         (\ _a91016 ->
                                                                                          case_ (_a15
                                                                                                 == case_ (_a91016
                                                                                                           == docUpdate3)
                                                                                                          [branch ptrue
                                                                                                                  (zNew611),
                                                                                                           branch pfalse
                                                                                                                  (z1
                                                                                                                   ! _a91016)])
                                                                                                [branch ptrue
                                                                                                        (nat_ 1),
                                                                                                 branch pfalse
                                                                                                        (nat_ 0)]) *
                                                                                 nat_ 2)))) *
                                           (nat2prob (summate (nat_ 0)
                                                              (size t2)
                                                              (\ _a91017 ->
                                                               case_ (_a91017 == docUpdate3)
                                                                     [branch ptrue (nat_ 0),
                                                                      branch pfalse
                                                                             (case_ (zNew611
                                                                                     == z1
                                                                                        ! _a91017)
                                                                                    [branch ptrue
                                                                                            (nat_ 1),
                                                                                     branch pfalse
                                                                                            (nat_ 0)])])) +
                                            as0 ! zNew611) *
                                           recip (nat2prob (summate (nat_ 0)
                                                                    (size t2)
                                                                    (\ _a91018 ->
                                                                     case_ (_a91018 == docUpdate3)
                                                                           [branch ptrue (nat_ 0),
                                                                            branch pfalse
                                                                                   (case_ (z1
                                                                                           ! _a91018
                                                                                           < nat_ 0)
                                                                                          [branch ptrue
                                                                                                  (nat_ 0),
                                                                                           branch pfalse
                                                                                                  (nat_ 1)])])) +
                                                  summate (nat_ 0)
                                                          (size as0)
                                                          (\ _a91019 -> as0 ! _a91019))
