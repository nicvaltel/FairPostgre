{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Domain.Connector (Connector(..)) where

import Domain.Types

class Monad m => Connector m where
  getDBConnectionInfo :: m DBConnectionInfo
  connect :: m DBConnection
  closeConnection :: DBConnection -> m ()
