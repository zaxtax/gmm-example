{-# LANGUAGE DataKinds, NegativeLiterals #-}
module Main where

import           Prelude                          hiding (product)
import           Language.Hakaru.Runtime.Prelude
import           Language.Hakaru.Types.Sing
import qualified System.Random.MWC                as MWC
import           Control.Monad
import           Data.Number.LogFloat hiding (product)

prog = 
  lam $ \ as0 ->
  lam $ \ z1 ->
  lam $ \ t2 ->
  lam $ \ docUpdate3 ->
  case_ (size z1 == size t2 && docUpdate3 < size z1)
        [branch ptrue
                ((pose (nat2prob (nat_ 2)
                        ** (nat2real (size as0) * fromProb (recip (nat2prob (nat_ 2))) +
                            nat2real (size t2) *
                            fromInt (int_ -1) *
                            fromProb (recip (nat2prob (nat_ 2)))) *
                        pi
                        ** (nat2real (size t2) *
                            fromInt (int_ -1) *
                            fromProb (recip (nat2prob (nat_ 2)))) *
                        exp (summate (nat_ 0)
                                     (size t2)
                                     (\ _a9104 -> t2 ! _a9104 ^ nat_ 2) *
                             fromInt (int_ -1) *
                             fromProb (recip (nat2prob (nat_ 2)))) *
                        product (nat_ 0)
                                (size as0)
                                (\ _a5 ->
                                 product (nat_ 0)
                                         (summate (nat_ 0)
                                                  (size t2)
                                                  (\ _a9107 ->
                                                   case_ (_a9107 == docUpdate3)
                                                         [branch ptrue (nat_ 0),
                                                          branch pfalse
                                                                 (case_ (_a5 == z1 ! _a9107)
                                                                        [branch ptrue (nat_ 1),
                                                                         branch pfalse (nat_ 0)])]))
                                         (\ j6 ->
                                          (nat2prob j6 + as0 ! _a5) *
                                          recip (product (nat_ 0)
                                                         (summate (nat_ 0)
                                                                  (size t2)
                                                                  (\ _a9109 ->
                                                                   case_ (_a9109 == docUpdate3)
                                                                         [branch ptrue (nat_ 0),
                                                                          branch pfalse
                                                                                 (case_ (z1 ! _a9109
                                                                                         < nat_ 0)
                                                                                        [branch ptrue
                                                                                                (nat_ 0),
                                                                                         branch pfalse
                                                                                                (nat_ 1)])]))
                                                         (\ _a8 ->
                                                          nat2prob _a8 +
                                                          summate (nat_ 0)
                                                                  (size as0)
                                                                  (\ _a91010 ->
                                                                   as0
                                                                   ! _a91010)))))) $
                       (categorical (array (size as0) $
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
                                                          (\ _a91019 -> as0 ! _a91019)))))),
         branch pfalse (reject)]

main :: IO ()
main = do
  g <- MWC.createSystemRandom
  print prog
