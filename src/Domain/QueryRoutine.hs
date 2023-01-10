{-# LANGUAGE OverloadedStrings #-}
module Domain.QueryRoutine  where


import Control.Monad ( replicateM_ )
import Control.Monad.Reader ( asks, MonadIO(liftIO), MonadReader(local, ask), ReaderT )
import Control.Exception ( catch, SomeException )
import qualified Data.ByteString.Char8 as BC




