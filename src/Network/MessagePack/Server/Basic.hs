{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Trustworthy                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-------------------------------------------------------------------
-- |
-- Module    : Network.MessagePackRpc.Server
-- Copyright : (c) Hideyuki Tanaka, 2010-2015
-- License   : BSD3
--
-- Maintainer:  tanaka.hideyuki@gmail.com
-- Stability :  experimental
-- Portability: portable
--
-- This module is server library of MessagePack-RPC.
-- The specification of MessagePack-RPC is at
-- <http://redmine.msgpack.org/projects/msgpack/wiki/RPCProtocolSpec>.
--
-- A simple example:
--
-- > import Network.MessagePackRpc.Server
-- >
-- > add :: Int -> Int -> Server Int
-- > add x y = return $ x + y
-- >
-- > main = serve 1234 [ method "add" add ]
--
--------------------------------------------------------------------

module Network.MessagePack.Server.Basic (
  -- * RPC method types
    Method
  , MethodVal (..)
  , MethodDocs (..)
  , MethodType (..)
  , ServerT (..)
  , Server

  -- * Build a method
  , method

  -- * Get the method name
  , methodName
  , methodDocs

  -- * Start RPC server
  , serve
  ) where

import           Control.Applicative                    (Alternative (..),
                                                         Applicative, pure,
                                                         (<$>), (<|>))
import           Control.Monad.Catch                    (Handler (..),
                                                         MonadCatch, MonadThrow,
                                                         SomeException, catch,
                                                         catches, throwM)
import           Control.Monad.Trans                    (MonadIO, MonadTrans,
                                                         lift, liftIO)
import           Control.Monad.Trans.Control            (MonadBaseControl)
import qualified Data.Binary                            as Binary
import qualified Data.ByteString                        as S
import           Data.Conduit                           (ResumableSource, Sink,
                                                         ($$), ($$+), ($$++))
import qualified Data.Conduit.Binary                    as CB
import           Data.Conduit.Network                   (appSink, appSource,
                                                         runGeneralTCPServer,
                                                         serverSettings,
                                                         setAfterBind)
import           Data.Conduit.Serialization.Binary      (ParseError, sinkGet)
import           Data.Either
import qualified Data.List                              as List
import           Data.MessagePack                       (MessagePack, Object,
                                                         fromObject, toObject)
-- import qualified Data.MessagePack.Result                as R
import           Data.Monoid                            ((<>))
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import           Data.Traversable                       (sequenceA)
import           Network.Socket                         (SocketOption (ReuseAddr),
                                                         setSocketOption)

import           Network.MessagePack.Interface.Internal (IsReturnType (..),
                                                         IsReturnTypeIO (..),
                                                         Returns)
import           Network.MessagePack.Types


newtype ServerT m a = ServerT { runServerT :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)


instance MonadTrans ServerT where
  lift = ServerT
  {-# INLINE lift #-}


type Server = ServerT IO


instance (MonadThrow m, MessagePack o, MethodType m r) => MethodType m (o -> r) where
  toBody n f (x : xs) =
    case fromObject x of
      Nothing -> throwM $ methodError "argument type error"
      Just r  -> toBody n (f r) xs
  toBody _ _ [] = throwM $ methodError "not enough arguments"

instance (Functor m, MonadThrow m, MessagePack o) => MethodType m (ServerT m o) where
  toBody _ m [] = toObject <$> runServerT m
  toBody n _ ls =
    throwM $ methodError $
      "invalid arguments for method '" <> n <> "': " <> T.pack (show ls)

-- Pure server
instance Monad m => IsReturnType m (Returns r) where
  type HaskellType (Returns r) = r
  type ServerType m (Returns r) = ServerT m r

  implement _ = return

-- IO Server
instance MonadIO m => IsReturnTypeIO m (Returns r) where
  type HaskellTypeIO (Returns r) = IO r
  type ServerTypeIO m (Returns r) = ServerT m r

  implementIO _ = liftIO


processRequests
  :: (Applicative m, MonadThrow m, MonadCatch m)
  => [Method m]
  -> ResumableSource m S.ByteString
  -> Sink S.ByteString m t
  -> m b
processRequests methods rsrc sink = do
  (rsrc', res) <- rsrc $$++ do
    obj <- sinkGet Binary.get
    case unpackRequest obj of
      Nothing ->
        throwM $ internalError "invalid request"
      Just req@(_, msgid, _, _) ->
        let
            handlers = [ Handler $ return . handleServerError msgid
                       , Handler $ return . handleAnyError msgid ]
        in lift $ getResponse methods req `catches` handlers

  _ <- CB.sourceLbs (packResponse res) $$ sink
  processRequests methods rsrc' sink

  where
    handleServerError :: Int -> ServerError -> Response
    handleServerError msgid err = (1, msgid, toObject err, toObject ())

    handleAnyError :: Int -> SomeException -> Response
    handleAnyError msgid exc = let err = internalError $ T.pack $ show exc
                                in (1, msgid, toObject err, toObject ())

getResponse
  :: Applicative m
  => [Method m]
  -> Request Object
  -> m Response
getResponse methods (0, msgid, mth, args) =
  process <$> callMethod methods mth args
  where
    process (Left  err) = (1, msgid, toObject err, toObject ())
    process (Right ok ) = (1, msgid, toObject (), ok)
    -- process (R.Failure err) = (1, msgid, toObject err, toObject ())
    -- process (R.Success ok ) = (1, msgid, toObject (), ok)

getResponse _ (rtype, msgid, _, _) =
  let err = internalError $ "request type is not 0, got " <> T.pack (show rtype)
  in pure (1, msgid, toObject err, toObject ())


callMethod
  :: (Applicative m)
  => [Method m]
  -> Object
  -> [Object]
  -> m (Either ServerError Object)
  -- -> m (Either ServerError Object)
callMethod methods mth args = sequenceA
    (stringCall =<< fromObject mth)
    -- <|>
    -- (intCall =<< fromObject mth)
  where
    stringCall name =
      case List.find ((== name) . methodName) methods of
        Nothing -> Left  $ methodNotFoundError name
        Just m  -> Right $ methodBody m args

    -- intCall ix =
    --   case drop ix methods of
    --     []  -> Left  $ methodNotFoundError $ "#" <> T.pack (show ix)
    --     m:_ -> Right $ methodBody m args


ignoreParseError :: Applicative m => ParseError -> m ()
ignoreParseError _ = pure ()


-- | Start RPC server with a set of RPC methods.
serve
  :: (MonadBaseControl IO m, MonadIO m, MonadCatch m)
  => Int        -- ^ Port number
  -> [Method m] -- ^ list of methods
  -> m ()
serve port methods =
  runGeneralTCPServer settings $ \ad -> do
    (rsrc, _) <- appSource ad $$+ return ()
    processRequests methods rsrc (appSink ad) `catch` ignoreParseError

  where
    settings =
      setAfterBind
        (\s -> setSocketOption s ReuseAddr 1)
        (serverSettings port "*")
