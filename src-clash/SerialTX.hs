{-# LANGUAGE RecordWildCards, TupleSections #-}
module SerialTX where

import Clash.Prelude
import Util
import SevenSegment

import Control.Category ((>>>))
import Control.Monad.State
import Data.Word
import Data.Int
import Data.Bits

{-# NOINLINE topEntity #-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "SerialTX"
    , t_inputs =
          [ PortName "CLK_32MHZ"
          , PortName "RESET"
          , PortName "SWITCH"
          ]
    , t_output = PortProduct ""
          [ PortProduct "" [PortName "SS_ANODES", PortName "SS_SEGS", PortName "SS_DP"]
          , PortName "TX"
          ]
    }) #-}
topEntity
    :: Clock System Source
    -> Reset System Asynchronous
    -> Signal System (Vec 8 Bit)
    -> ((Signal System (Vec 4 Bit), Signal System (Vec 7 Bit), Signal System Bit), Signal System Bit)
topEntity clk rst = exposeClockReset board clk (activeLowReset rst) -- (unsafeToAsyncReset $ pure False) -- (activeLowReset rst)

board
    :: (HiddenClockReset domain gated synchronous)
    => Signal domain (Vec 8 Bit)
    -> ((Signal domain (Vec 4 Bit), Signal domain (Vec 7 Bit), Signal domain Bit), Signal domain Bit)
board switches = ((ssAnodes <$> ss', ssSegments <$> ss', ssDP <$> ss'), txOut <$> tx (Just <$> userInput))
  where
    userInput = unpack . v2bv <$> switches
    (hi, lo) = unbundle $ splitByte <$> userInput
    digits = lo :> hi :> pure 0 :> pure 0 :> Nil
    ss = driveSS 10000 (bundle digits)
    ss' = activeLow <$> ss

    noSegs = pure (repeat low)

clkRate :: Word32
clkRate = 32000000

-- testSS = sampleN 50 $
--          (\ ((anodes, segs, _), _) -> anodes) $
--          board $ fromList $
--          fmap (bv2v . (pack :: Word8 -> BitVector 8)) [0..10]
--   where
--     display ((anodes, segs, _), _) = putStrLn (showSS segs) >> putStrLn "------------------"

data TXState = TXIdle
             | TXStart
             | TXBit (Index 8)

data TXOut = TXOut{ txReady :: Bool, txOut :: Bit }

serialRate :: Word32
serialRate = 9600

tx0 :: Maybe Word8 -> State (Maybe (Word8, TXState)) TXOut
tx0 v = do
    s <- get
    case s of
        Nothing -> do
            traverse (\v -> put $ Just (v, TXIdle)) v
            return $ TXOut True high
        Just (v, s) -> TXOut False <$> case s of
            TXIdle -> do
                put $ Just (v, TXStart)
                return high
            TXStart -> do
                put $ Just (v, TXBit 0)
                return low
            TXBit i -> do
                put $ ((v,) . TXBit) <$> succIdx i
                return $ pack v ! i

stepped :: Bool -> State s a -> State s a
stepped allow act = do
    s0 <- get
    res <- act
    unless allow $ put s0
    return res

tx
    :: (HiddenClockReset domain gated synchronous)
    => Signal domain (Maybe Word8) -> Signal domain TXOut
tx = mealyState' tx0 Nothing (countTo $ clkRate `div` serialRate)
