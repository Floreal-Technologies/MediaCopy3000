module MediaCopy.Guard
  ( guarded
  ) where

import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)

guarded :: IO a -> IO (Either SomeException a)
guarded action =
  try @SomeException action >>= \case
    Left e
      | Just _ <- fromException @SomeAsyncException e -> throwIO e
      | otherwise -> pure (Left e)
    Right value -> pure (Right value)
