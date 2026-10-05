module MediaCopy.Signals
  ( onStopSignal
  ) where

onStopSignal :: IO () -> IO ()
onStopSignal _ = pure ()
