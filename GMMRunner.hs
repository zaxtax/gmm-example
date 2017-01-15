{-# LANGUAGE DataKinds
           , FlexibleContexts
           , NegativeLiterals
           , ForeignFunctionInterface
           #-}

module Main where

import           Prelude                          hiding (product)
import           Language.Hakaru.Runtime.Prelude
import           Language.Hakaru.Types.Sing
import           System.CPUTime
import           Data.Time.Clock
import           System.Environment
import           System.IO.Unsafe
import qualified System.Random.MWC                as MWC
import qualified System.Random.MWC.Distributions  as MWCD
import           Control.Monad
import qualified Data.Vector                      as V
import qualified Data.Vector.Generic              as G
import qualified Data.Vector.Unboxed             as U
import qualified Data.Vector.Storable             as SV


as = G.fromList [1.0, 1.0, 1.0]

gmmGibbs = 
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
                                     (\ _a9104 ->
                                      t2 ! _a9104 ^ nat_ 2 *
                                      fromInt (int_ -1) *
                                      fromProb (recip (nat2prob (nat_ 2))))) *
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
                                                                                (nat2real (nat_ 0))]
                                                                  ^ nat_ 2) *
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
                                                                                                           (nat_ 0)])))) *
                                                         fromProb (recip (nat2prob (nat_ 2))))) *
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
                                                                                                        (nat_ 0)] *
                                                                                          nat_ 2))))) *
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

gmmGibbs2 =
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

        
dataSize = 3000

t_ = 
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
  let_ (nat_ dataSize) $ \ data_size10 ->
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

        
zInit_ = 
  (plate (nat_ dataSize) $
         \ i0 ->
         categorical (array (nat_ 3) $
                            \ x1 ->
                            case_ (x1 == nat_ 0)
                                  [branch ptrue (nat2prob (nat_ 1)),
                                   branch pfalse
                                          (case_ (x1 == nat_ 1)
                                                 [branch ptrue (nat2prob (nat_ 1)),
                                                  branch pfalse (nat2prob (nat_ 1))])]))

        
type Index = Int

iterateM :: Monad m => Int -> (a -> b -> Int -> m b) -> a -> b -> m b
iterateM 0 _ _ b = return b
iterateM n f a b = f a b (n - 1) >>= iterateM (n - 1) f a

oneUpdate
    :: (MWC.GenIO, U.Vector Double)
    -> U.Vector Int
    -> Int
    -> IO (U.Vector Int)
oneUpdate (g,t) z i = do
    -- print z
    Just zNew <- unMeasure (gmmGibbs2 as z t i) g
    return (G.unsafeUpd z [(i, zNew)])

oneSweep
    :: MWC.GenIO
    -> U.Vector Int
    -> U.Vector Double
    -> IO (U.Vector Int)
oneSweep g z t = iterateM size oneUpdate (g, t) z
  where size = G.length z
        
main = do
  g  <- MWC.createSystemRandom
  Just z  <- unMeasure zInit_ g
  Just t' <- unMeasure t_ g
  oneSweep g z t' >>= print
