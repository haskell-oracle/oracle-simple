------------------------------------------------------------------------------

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
module Database.Oracle.Simple
  ( -- * Writing queries
    -- $use
    -- $querytype
    -- $subst
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
    -- $queue
  , module Queue
  , module Object

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

    -- * Miscellaneous
  , module Export
  , module LOB
  ) where

import Database.Oracle.Simple.Execute as Execute
import Database.Oracle.Simple.FromField as FromField
import Database.Oracle.Simple.FromRow as FromRow
import Database.Oracle.Simple.Internal as Connection
  ( AdditionalConnectionParams (..)
  , Connection (..)
  , ConnectionCreateParams (..)
  , ConnectionParams (..)
  , DPIConn (..)
  , DPIPool (..)
  , DPIPoolCreateParams (..)
  , close
  , connect
  , defaultAdditionalConnectionParams
  , withConnection
  , withDefaultPoolCreateParams
  )
import Database.Oracle.Simple.Internal as Error
  ( ErrorInfo (..)
  , OracleError (..)
  , renderErrorInfo
  , throwOracleError
  )
import Database.Oracle.Simple.Internal as Export hiding
  ( AdditionalConnectionParams (..)
  , Connection (..)
  , ConnectionCreateParams (..)
  , ConnectionParams (..)
  , DPIConn (..)
  , DPIPool (..)
  , DPIPoolCreateParams (..)
  , ErrorInfo (..)
  , OracleError (..)
  , connect
  , close
  , defaultAdditionalConnectionParams
  , renderErrorInfo
  , throwOracleError
  , withConnection
  , withDefaultPoolCreateParams
  )
import Database.Oracle.Simple.JSON as JSON
import Database.Oracle.Simple.Object as Object
import Database.Oracle.Simple.Pool as Pool
import Database.Oracle.Simple.Query as Query
import Database.Oracle.Simple.Queue as Queue
import Database.Oracle.Simple.ToField as ToField
import Database.Oracle.Simple.ToRow as ToRow
import Database.Oracle.Simple.Transaction as Transaction
import Database.Oracle.Simple.LOB as LOB

-- $use
-- This library provides a 'Query' type and a parameter substitution
-- facility to address both ease of use and security.

-- $querytype
--
-- To most easily construct a query and write your query as a normal literal string.
--
-- > import Database.Oracle.Simple
-- >
-- > main :: IO ()
-- > main = do
-- >   let stmt = "select 2 + 2"
-- >   conn <- connect (ConnectionParams "username" "password" "localhost/Free" Nothing)
-- >   rows <- query_ conn stmt :: IO [Only Double]
-- >   print rows
--
-- A 'Query' value does not represent the actual query that will be
-- executed, but is a template for constructing the final query.

-- $subst
--
-- Since applications need to be able to construct queries with
-- parameters that change, this library provides a query substitution
-- capability.
-- For example,
--
-- > {# LANGUAGE TypeApplications #}
-- >
-- > main :: IO ()
-- > main = do
-- >   conn <- connect (ConnectionParams "username" "password" "localhost/Free" Nothing)
-- >   void $ execute_ conn "create table test(text_column number(10,0) primary key)"
-- >   void $ execute conn "insert into test values(:1)" (Only @Int 1)
-- >   results <- query_ conn "select * from test" :: IO [Only Int]
-- >   print result
--
-- Output:
--
-- > [Only {fromOnly = 1}]

-- $queue
-- 
-- Oracle Database Advanced Queuing provides database-integrated message queuing functionality. 
-- It is built on top of Oracle Streams and leverages the functions of Oracle Database so that messages can be stored persistently, 
-- propagated between queues on different computers and databases, and transmitted using Oracle Net Services and HTTP(S).
-- Because Oracle Database Advanced Queuing is implemented in database tables, all operational benefits of high availability, 
-- scalability, and reliability are also applicable to queue data. Standard database features such as recovery, restart, 
-- and security are supported by Oracle Database Advanced Queuing. You can use database development and management tools 
-- such as Oracle Enterprise Manager to monitor queues. Like other database tables, queue tables can be imported and exported.
-- Messages can be queried using standard SQL. This means that you can use SQL to access the message properties, 
-- the message history, and the payload. With SQL access you can also audit and track messages.
-- All available SQL technology, such as indexes, can be used to optimize access to messages
--
-- > import Database.Oracle.Simple
-- > 
-- > params :: ConnectionParams
-- > params = ConnectionParams "username" "password" "localhost:1521/free" Nothing
-- > 
-- > main :: IO ()
-- > main = do
-- >   conn <- connect params
-- >   msgProps <- genMsgProps conn
-- >   queue <- genQueue conn "TEST_QUEUE"
-- >   
-- >   setMsgPropsPayLoadBytes msgProps (BSC.pack "Hello from Haskell!")
-- >   
-- >   void $ enqOne queue msgProps
-- >   newMsgProps <- deqOne queue
-- >   mPayload <- getMsgPropsPayLoadBytes newMsgProps
-- >   
-- >   print mPayload
-- >   queueRelease queue
--
