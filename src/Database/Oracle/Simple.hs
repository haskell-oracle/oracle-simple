------------------------------------------------------------------------------
-- |
-- Module:      Database.Oracle.Simple
-- Copyright:   H-E-B (c) 2024.
-- License:     BSD3
-- Maintainer:  David Johnson <djohnson.m@gmail.com>, khandkararjun@gmail.com
-- Stability:   experimental
--
-- A mid-level client library for the Oracle database, aimed at ease of
-- use and high performance. 
--
-- Modern bindings to Oracle odpic C library.
--
------------------------------------------------------------------------------

module Database.Oracle.Simple (
    -- * Writing queries
    -- $use
    -- ** The Query type
    -- $querytype
    -- ** Parameter substitution
    -- $subst
    -- * Queries that return results
    module Query
    -- * Statements that do not return results
  , module Execute
  -- * Connection management
  , module Connection
  -- ** Pool connection
  , module Pool
  -- * Transaction handling
  , module Transaction
  -- * Advanced Queuing
  , module Queue
  , module QueueInternal
  -- * Error Handling
  , module Error
  -- * ToField TypeClass
  , module ToField
  -- * FromField TypeClass
  , module FromField
  -- * JSON Support
  , module JSON 
  -- * FromRow Instance
  , module FromRow
  -- * ToRow Instance
  , module ToRow
  -- * miscellaneous
  , module Export
) where

import Database.Oracle.Simple.Execute as Export
import Database.Oracle.Simple.FromField as Export
import Database.Oracle.Simple.FromRow as Export
import Database.Oracle.Simple.Internal as Export
import Database.Oracle.Simple.JSON as Export
import Database.Oracle.Simple.Pool as Export
import Database.Oracle.Simple.Query as Export
import Database.Oracle.Simple.ToField as Export
import Database.Oracle.Simple.ToRow as Export
import Database.Oracle.Simple.Transaction as Export
import Database.Oracle.Simple.Queue as Export
