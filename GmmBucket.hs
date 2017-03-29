{-# LANGUAGE DataKinds, NegativeLiterals #-}
module GmmBucket where

import           Data.Number.LogFloat hiding (product)
import           Prelude              hiding (product, exp, log, (**))

import           Language.Hakaru.Runtime.LogFloatPrelude
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
                                   (let_ (bucket (nat_ 0)
                                                 (size t2)
                                                 ((r_split (\ (i7,()) -> i7 == docUpdate3)
                                                           (r_fanout (r_add (\ (i7,()) -> t2 ! i7))
                                                                     r_nop)
                                                           (r_index (\ () -> size as0)
                                                                    (\ (i7,()) -> z1 ! i7)
                                                                    (r_add (\ (i7,(_a9108,())) ->
                                                                            t2
                                                                            ! i7)))))) $ \ summary6 ->
                                    case_ (_a9105 == zNew64)
                                          [branch ptrue
                                                  (case_ (case_ summary6
                                                                [branch (ppair PVar PVar)
                                                                        (\ y9 z10 -> y9)])
                                                         [branch (ppair PVar PVar)
                                                                 (\ y11 z12 -> y11)]),
                                           branch pfalse (real_ 0)] +
                                    case_ summary6 [branch (ppair PVar PVar) (\ y13 z14 -> z14)]
                                    ! _a9105)
                                   ^ nat_ 2 *
                                   fromProb (recip (nat2prob (nat_ 1 +
                                                              (let_ (bucket (nat_ 0)
                                                                            (size t2)
                                                                            ((r_split (\ (i16,()) ->
                                                                                       i16
                                                                                       == docUpdate3)
                                                                                      (r_fanout (r_add (\ (i16,()) ->
                                                                                                        nat_ 1))
                                                                                                r_nop)
                                                                                      (r_index (\ () ->
                                                                                                size as0)
                                                                                               (\ (i16,()) ->
                                                                                                z1
                                                                                                ! i16)
                                                                                               (r_add (\ (i16,(_a91017,())) ->
                                                                                                       nat_ 1)))))) $ \ summary15 ->
                                                               case_ (_a9105 == zNew64)
                                                                     [branch ptrue
                                                                             (case_ (case_ summary15
                                                                                           [branch (ppair PVar
                                                                                                          PVar)
                                                                                                   (\ y18
                                                                                                      z19 ->
                                                                                                    y18)])
                                                                                    [branch (ppair PVar
                                                                                                   PVar)
                                                                                            (\ y20
                                                                                               z21 ->
                                                                                             y20)]),
                                                                      branch pfalse (nat_ 0)] +
                                                               case_ summary15
                                                                     [branch (ppair PVar PVar)
                                                                             (\ y22 z23 -> z23)]
                                                               ! _a9105))))) *
                          real_ (1/2)) *
                     recip (nat_ 2
                            `thRootOf` (nat2prob (product (nat_ 0)
                                                          (size as0)
                                                          (\ _a24 ->
                                                           nat_ 2 +
                                                           (let_ (bucket (nat_ 0)
                                                                         (size t2)
                                                                         ((r_split (\ (_a91026,()) ->
                                                                                    _a91026
                                                                                    == docUpdate3)
                                                                                   (r_fanout (r_add (\ (_a91026,()) ->
                                                                                                     nat_ 1))
                                                                                             r_nop)
                                                                                   (r_index (\ () ->
                                                                                             size as0)
                                                                                            (\ (_a91026,()) ->
                                                                                             z1
                                                                                             ! _a91026)
                                                                                            (r_add (\ (_a91026,(_a27,())) ->
                                                                                                    nat_ 1)))))) $ \ summary25 ->
                                                            case_ (_a24 == zNew64)
                                                                  [branch ptrue
                                                                          (case_ (case_ summary25
                                                                                        [branch (ppair PVar
                                                                                                       PVar)
                                                                                                (\ y28
                                                                                                   z29 ->
                                                                                                 y28)])
                                                                                 [branch (ppair PVar
                                                                                                PVar)
                                                                                         (\ y30
                                                                                            z31 ->
                                                                                          y30)]),
                                                                   branch pfalse (nat_ 0)] +
                                                            case_ summary25
                                                                  [branch (ppair PVar PVar)
                                                                          (\ y32 z33 -> z33)]
                                                            ! _a24) *
                                                           nat_ 2)))) *
                     ((let_ (bucket (nat_ 0)
                                    (size t2)
                                    ((r_split (\ (_a91035,()) -> _a91035 == docUpdate3)
                                              r_nop
                                              (r_index (\ () -> size as0)
                                                       (\ (_a91035,()) -> z1 ! _a91035)
                                                       (r_add (\ (_a91035,(zNew636,())) ->
                                                               nat_ 1)))))) $ \ summary34 ->
                       nat2prob (case_ summary34
                                       [branch (ppair PVar PVar) (\ y37 z38 -> z38)]
                                 ! zNew64)) +
                      as0 ! zNew64) *
                     recip (nat2prob (summate (nat_ 0)
                                              (size t2)
                                              (\ _a91039 ->
                                               case_ (_a91039 == docUpdate3)
                                                     [branch ptrue (nat_ 0),
                                                      branch pfalse
                                                             (case_ (z1 ! _a91039 < nat_ 0)
                                                                    [branch ptrue (nat_ 0),
                                                                     branch pfalse (nat_ 1)])])) +
                            summate (nat_ 0) (size as0) (\ _a91040 -> as0 ! _a91040)))
