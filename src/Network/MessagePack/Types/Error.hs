{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
module Network.MessagePack.Types.Error
  ( RpcError (..)
  , ServerError (..)
  , rpcError
  , internalError
  , methodError
  , methodNotFoundError
  ) where

import           Control.Exception (Exception)
import           Data.MessagePack  (MessagePack (..), Object)
import           Data.Monoid       ((<>))
import           Data.Text         (Text)
import           Data.Typeable     (Typeable)
import           GHC.Generics


rpcError :: MessagePack o => Text -> o -> ServerError
rpcError path x = ServerError ("." <> path) $ toObject x

methodError :: Text -> ServerError
methodError = rpcError "CallError"

methodNotFoundError :: Text -> ServerError
methodNotFoundError methodName = rpcError "CallError.NoMethodError" $
    "Method `" <> methodName <> "` is not found"

internalError :: Text -> ServerError
internalError = rpcError "InternalError"

-- | RPC error type
data RpcError
  = RemoteError !Object           -- ^ Server error
  | ResultTypeError !Text !Object -- ^ Result type mismatch
  | ProtocolError !Text           -- ^ Protocol error
  deriving (Show, Eq, Ord, Typeable)

instance Exception RpcError


data ServerError = ServerError
  { errorCode :: !Text
  , errorBody :: !Object
  } deriving (Show, Typeable, Generic)

instance Exception ServerError
instance MessagePack ServerError
