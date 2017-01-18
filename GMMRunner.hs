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

import GmmGibbs
import GmmGibbs2

as = G.fromList [1.0, 1.0, 1.0]        
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
