{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingVia #-}
-- | "Servant"
module Scratchpad
( module Servant
, module Servant.Server
, module Data.WorldPeace
) where


import           Data.String.Conversions
                 (cs)
import qualified Data.ByteString                            as B
import Data.Monoid (First(..))
import Data.Kind
import Data.Proxy
import GHC.TypeLits
import Data.Maybe
import Servant
import Servant.Server
import Servant.API
import Servant.API.ContentTypes
import Servant.API.Generic
import Data.Aeson.Types
import Data.WorldPeace hiding (Nat)
import qualified Data.ByteString                            as B

import Data.SOP.NS
import Data.SOP.Classes
import Data.SOP.Constraint
import Data.SOP.BasicFunctors
import           GHC.TypeLits
                 (KnownNat, KnownSymbol, natVal, symbolVal)
import qualified Network.HTTP.Media                         as NHM
import           Network.HTTP.Types                         hiding
                 (Header, ResponseHeaders)
import           Network.Socket
                 (SockAddr)
import           Network.Wai
                 (Application, Request, httpVersion, isSecure, lazyRequestBody,
                 rawQueryString, remoteHost, requestBody, requestHeaders,
                 requestMethod, responseLBS, responseStream, vault)
import           Servant.API
                 ((:<|>) (..), (:>), Accept (..), BasicAuth, Capture',
                 CaptureAll, Description, EmptyAPI, FramingRender (..),
                 FramingUnrender (..), FromSourceIO (..), Header', If,
                 IsSecure (..), QueryFlag, QueryParam', QueryParams, Raw,
                 ReflectMethod (reflectMethod), RemoteHost, ReqBody',
                 SBool (..), SBoolI (..), SourceIO, Stream, StreamBody',
                 Summary, ToSourceIO (..), Vault, Verb, WithNamedContext)
import           Servant.API.ContentTypes
                 (AcceptHeader (..), AllCTRender (..), AllCTUnrender (..),
                 AllMime, MimeRender (..), MimeUnrender (..), canHandleAcceptH)
import           Servant.API.Modifiers
                 (FoldLenient, FoldRequired, RequestArgument,
                 unfoldRequestArgument)
import           Servant.API.ResponseHeaders
                 (GetHeaders, Headers, getHeaders, getResponse)
import qualified Servant.Types.SourceT                      as S
import           Web.HttpApiData
                 (FromHttpApiData, parseHeader, parseQueryParam,
                 parseUrlPieceMaybe, parseUrlPieces, parseUrlPiece)


import           Servant.Server.Internal
import           Servant.Server.Internal.Delayed
import           Servant.Server.Internal.DelayedIO
import           Servant.Server.Internal.Handler
import           Servant.Server.Internal.Router
import           Servant.Server.Internal.RouteResult
import           Servant.Server.Internal.RoutingApplication
import           Servant.Server.Internal.ServerError



newtype Status' n a = Status' a

class HasStatus a where
  getStatus :: a -> Status

instance forall n a. KnownNat n => HasStatus (Status' n a) where
  getStatus _ = toEnum $ fromInteger $  natVal (Proxy  @n)
  
data NotFound = NotFound { msg :: String }
  deriving stock (Generic)
  deriving anyclass (ToJSON)
  deriving HasStatus via (Status' 404 NotFound)

data UserUnauthorized = UserUnauthorized { msg :: String } 
  deriving stock (Generic)
  deriving anyclass (ToJSON)
  deriving HasStatus via (Status' 401 UserUnauthorized)

data UserView = UserView { name :: String }
  deriving stock (Generic)
  deriving anyclass (ToJSON)
  deriving HasStatus via (Status' 200 UserView)

data UserCreated = UserCreated { name :: String }
  deriving stock (Generic)
  deriving anyclass (ToJSON)
  deriving HasStatus via (Status' 201 UserCreated)


  


-- Currently, all return types must implement the same encodings.
-- I think this is a sane limitation
data Verb' (method :: StdMethod) (contentTypes :: [*]) (returns :: [*])

type Get' = Verb' GET
type Put' = Verb' PUT

data Routes route = Routes
  { get :: route :- Capture "id" Int 
         :> Get' '[JSON] '[ UserView , NotFound ]
  , put :: route :- ReqBody '[JSON] Int 
         :> Put' '[JSON] '[ UserCreated, UserUnauthorized ]
  }
  deriving (Generic)



type Both cts a = (AllCTRender cts a, HasStatus a)


class (AllCTRender cts a, HasStatus a) => HasAll cts a where
instance (AllCTRender cts a, HasStatus a) => HasAll cts a where

instance (AllMime cts, All (HasAll cts) returns, ReflectMethod method) => HasServer (Verb' method cts returns) context where
  type ServerT (Verb' method cts returns) m = m  (NS I returns)

  hoistServerWithContext _ _ nt s = nt s
  route Proxy ctx action =  leafRouter route'
    where
      method = reflectMethod (Proxy @method)
      route' env request respond =
        runAction action'  env request respond $ \ output -> do
           let (status, b') = collapse_NS . cmap_NS (Proxy @(HasAll cts)) (\(I b) -> K (getStatus b, handleAcceptH (Proxy @cts) (AcceptHeader accH) b)) $ output
           case b' of
             Nothing -> FailFatal err406 -- this should not happen (checked before), so we make it fatal if it does
             Just (contentT, body) ->
              let bdy = if allowedMethodHead method request then "" else body
              in Route $ responseLBS status ((hContentType, cs contentT) : []) bdy
        where
          accH = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
          action' = action `addMethodCheck` methodCheck method request
                           `addAcceptCheck` acceptCheck (Proxy @cts) accH

