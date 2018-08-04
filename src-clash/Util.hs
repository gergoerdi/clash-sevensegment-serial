module Util
       ( mealyState
       , mealyState'
       , activeLowReset
       , activeLow
       , activeHigh
       , countTo
       , muxRR
       , nextIdx
       , succIdx
       ) where

import Clash.Prelude
import Control.Monad.State
import Data.Word
import Data.Maybe (fromMaybe)

mealyState :: (HiddenClockReset domain gated synchronous)
           => (i -> State s o) -> s -> (Signal domain i -> Signal domain o)
mealyState f s = mealyState' f s (pure True)

mealyState' :: (HiddenClockReset domain gated synchronous)
            => (i -> State s o) -> s -> Signal domain Bool -> Signal domain i -> Signal domain o
mealyState' f s = curry $ mealy step s . bundle
  where
    step s (progress, x) = let (y, s') = runState (f x) s
                           in (if progress then s' else s, y)

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
    step (mask, i) True = (rotateLeftS mask d1, nextIdx i)

    mask0 = repeat False <<+ True

nextIdx :: (KnownNat n) => Index n -> Index n
nextIdx = fromMaybe 0 . succIdx

succIdx :: (KnownNat n) => Index n -> Maybe (Index n)
succIdx x | x == maxBound = Nothing
          | otherwise = Just $ succ x
