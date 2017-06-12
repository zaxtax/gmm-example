{-# LANGUAGE DataKinds
           , FlexibleContexts
           , NegativeLiterals
           , ForeignFunctionInterface
           #-}

module Main where

import           Prelude                          hiding (product, exp, log, (**))
import           Language.Hakaru.Runtime.LogFloatPrelude
import           Language.Hakaru.Types.Sing
import           System.CPUTime
import           Data.Time.Clock
import           System.Environment
import           System.IO.Unsafe
import qualified System.Random.MWC                as MWC
import qualified System.Random.MWC.Distributions  as MWCD
import           Control.Monad
import           Data.List                        (permutations)
import qualified Data.Vector                      as V
import qualified Data.Vector.Generic              as G
import qualified Data.Vector.Unboxed              as U
import qualified Data.Vector.Storable             as SV
import qualified Data.Number.LogFloat             as LF
import           Text.Printf                      (printf)


--import qualified GmmBase
--import GmmGibbs
import GmmGibbs2
--import GmmGibbs3
import qualified GmmBucket

clusters = 3
as = G.replicate clusters 1.0

diffToDouble :: NominalDiffTime -> Double
diffToDouble = fromRational . toRational

t_ dataSize = 
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
  let_ (arrayLit [ prob_ 1, prob_ 1, prob_ 1 ])  $ \ as8 ->
  let_ (nat_ dataSize) $ \ data_size10 ->
  dirichlet0 `app` as8 >>= \ theta11 ->
  let_ (arrayLit [ int_ -7, int_ 3, int_ 10 ]) $ \ phi12 ->
  (plate data_size10 $
         \ i14 ->
         categorical theta11 >>= \ z15 ->
         normal (fromInt (phi12 ! z15)) (nat2prob (nat_ 1)) >>= \ w16 ->
         dirac (pair z15 w16))

        
zInit_ dataSize =
  (plate (nat_ dataSize) $
         \ i0 ->
         categorical (arrayLit [ prob_ 1, prob_ 1, prob_ 1 ]))

accuracy
    :: U.Vector Int
    -> U.Vector Int
    -> Double
accuracy x y = G.sum z / (fromIntegral $ G.length x)
    where z = G.zipWith (\a b -> if a == b then 1 else 0) x y

accuracyWithPerm
    :: U.Vector Int
    -> U.Vector Int
    -> Double
accuracyWithPerm x y =
    maximum $
    map (\key -> accuracy x (relabel key y))
            (permutations [0 .. clusters - 1])

relabel :: [Int] -> U.Vector Int -> U.Vector Int
relabel key = G.map (key !!)

iterateM :: Monad m => Int -> (a -> b -> Int -> m b) -> a -> b -> m b
iterateM 0 _ _ b = return b
iterateM n f a b = f a b (n - 1) >>= iterateM (n - 1) f a

iterateM2 :: Monad m => Int -> (a -> Int ->  m a) -> a -> m a
iterateM2 0 _ a = return a
iterateM2 n f a = f a (n - 1) >>= iterateM2 (n - 1) f

oneUpdateB
    :: (MWC.GenIO, U.Vector Double)
    -> U.Vector Int
    -> Int
    -> IO (U.Vector Int)
oneUpdateB (g,t) z i = do
    Just zNew <- unMeasure (GmmBucket.prog as z t i) g
    --print (z, i, zNew)
    return (G.unsafeUpd z [(i, zNew)])

oneSweepB
    :: MWC.GenIO
    -> U.Vector Int
    -> U.Vector Double
    -> IO (U.Vector Int)
oneSweepB g z t = iterateM size oneUpdateB (g, t) z
  where size = G.length z

oneUpdate
    :: (MWC.GenIO, U.Vector Double)
    -> U.Vector Int
    -> Int
    -> IO (U.Vector Int)
oneUpdate (g,t) z i = do
    --print $ G.map LF.logFromLogFloat (gmmTestArray as z t i) -- DEBUG
    Just zNew <- unMeasure (gmmGibbs as z t i) g
    return (G.unsafeUpd z [(i, zNew)])

oneSweep
    :: MWC.GenIO
    -> U.Vector Int
    -> U.Vector Double
    -> IO (U.Vector Int)
oneSweep g z t = iterateM size oneUpdate (g, t) z
  where size = G.length z
      
main = do
  args <- getArgs
  case length args == 3 of
    False -> putStrLn "./gmmSweeps <dataSize> <sweeps> <trial>"
    True  -> do
        let [dataSize, sweeps, trial] = map read args :: [Int]
        g <- MWC.createSystemRandom
        Just z  <- unMeasure (zInit_ dataSize) g
        Just d  <- unMeasure (t_ dataSize) g
        let (zG, t') = G.unzip d
        --putStrLn ("zInit: " ++ show z) -- DEBUG
        --putStrLn ("Data: "  ++ show t') -- DEBUG

        iterateM2 sweeps (\z i -> do
            z' <- oneSweepB g z t'
            printf "Hakaru,%d,%d,%.6f\n"
                       trial
                       (sweeps - i)
                       (accuracyWithPerm zG z')
            return z') z
        return ()

        -- putStrLn ("Gibbs sampling time: " ++ show (diffToDouble $ diffUTCTime t2 t1))
        -- printf "Hakaru,%d,%d," sweeps trial
        -- print (accuracyWithPerm zG zPred)
