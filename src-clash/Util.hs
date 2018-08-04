module Util
       ( mealyState
       , activeLowReset
       , activeLow
       , activeHigh
       , countTo
       , muxRR
       ) where

import Clash.Prelude
import Control.Monad.State
import Data.Word

mealyState :: (HiddenClockReset domain gated synchronous)
           => (i -> State s o) -> s -> (Signal domain i -> Signal domain o)
mealyState f = mealy step
  where
    step s x = let (y, s') = runState (f x) s
               in (s', y)

activeLowReset :: Reset domain Asynchronous -> Reset domain Asynchronous
activeLowReset = unsafeToAsyncReset . (not <$>) . unsafeFromAsyncReset

activeLow :: (Functor f) => f Bool -> f Bit
activeLow = fmap complement . activeHigh

activeHigh :: (Functor f) => f Bool -> f Bit
activeHigh = fmap boolToBit

countTo
    :: (HiddenClockReset domain gated synchronous)
    => Word32 -> Signal domain Bool
countTo n = mealyState step 0 (pure ())
  where
    step () = do
        k <- get
        let k' = k + 1
            finished = k' == n
        put $ if finished then 0 else k'
        return finished

muxRR
    :: forall domain gated synchronous n a. (HiddenClockReset domain gated synchronous, KnownNat n)
    => Signal domain Bool
    -> Vec n (Signal domain a)
    -> (Signal domain (Vec n Bool), Signal domain a)
muxRR next ss = let (mask, i) = unbundle $ moore step id (mask0, (0 :: Index n)) next
                in (mask, (!!) <$> bundle ss <*> i)
  where
    step s False = s
    step (mask, i) True = (rotateLeftS mask d1, succIdx i)

    mask0 = repeat False <<+ True

succIdx :: (KnownNat n) => Index n -> Index n
succIdx x | x == maxBound = 0
          | otherwise = succ x
