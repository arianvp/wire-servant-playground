{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingVia #-}
-- | "Servant"
module Scratchpad where


import           Data.String.Conversions
                 (cs)
import qualified Data.ByteString                            as B
import Data.Monoid (First(..))
import Data.Kind
import Data.Proxy
import GHC.TypeLits
import Data.Maybe
import Servant hiding (And, Elem)
import Servant.Server
import Servant.Server.Generic
import Servant.API.ContentTypes
import Servant.API.Generic
import Data.Aeson.Types
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

data CreateUser = CreateUser { name :: String}
  deriving stock (Generic)
  deriving anyclass (ToJSON)

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
  , put :: route :- ReqBody '[JSON] CreateUser 
         :> Put' '[JSON] '[ UserCreated, UserUnauthorized ]
  }
  deriving (Generic)


server :: Routes AsServer
server = Routes
  { get = get'
  , put = put'
  }
  where
    get' :: Int -> Handler (NS I '[UserView, NotFound])
    get' x = 
      if False  -- not found
      then pureNS $ NotFound "Didn't find it"
      else pureNS $ UserView "fisx"

    put' ::  CreateUser -> Handler (NS I '[UserCreated, UserUnauthorized])
    put' (CreateUser name) = 
      if False -- unauthorized
      then pureNS $ UserUnauthorized "Nopeee!"
      else pureNS $ UserCreated name


pureNS :: (Applicative f, IsMember x xs) => x -> f (NS I xs)
pureNS = pure . inject'

inject' :: IsMember x xs => x -> NS I xs
inject' = inject . I

instance (AllMime cts, All (AllCTRender cts `And` HasStatus) returns, ReflectMethod method) => HasServer (Verb' method cts returns) context where
  type ServerT (Verb' method cts returns) m = m  (NS I returns)

  hoistServerWithContext _ _ nt s = nt s
  route Proxy ctx action =  leafRouter route'
    where
      method = reflectMethod (Proxy @method)
      route' env request respond =
        runAction action'  env request respond $ \ output -> do
           let (status, b') = collapse_NS . cmap_NS (Proxy @(AllCTRender cts `And` HasStatus)) (\(I b) -> K (getStatus b, handleAcceptH (Proxy @cts) (AcceptHeader accH) b)) $ output
           case b' of
             Nothing -> FailFatal err406 -- this should not happen (checked before), so we make it fatal if it does
             Just (contentT, body) ->
              let bdy = if allowedMethodHead method request then "" else body
              in Route $ responseLBS status ((hContentType, cs contentT) : []) bdy
        where
          accH = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
          action' = action `addMethodCheck` methodCheck method request
                           `addAcceptCheck` acceptCheck (Proxy @cts) accH













------------------- Stuff stolen from WorldPeace but for generics-sop

type IsMember (a :: u) (as :: [u]) = (CheckElemIsMember a as, UElem a as (RIndex a as))

type family Contains (as :: [k]) (bs :: [k]) :: Constraint where
  Contains '[] _ = ()
  Contains (a ': as) bs = (IsMember a bs, Contains as bs)

data Nat_ = S_ Nat_ | Z_


type family RIndex (r :: k) (rs :: [k]) :: Nat_ where
  RIndex r (r ': rs) = 'Z_
  RIndex r (s ': rs) = 'S_ (RIndex r rs)

type family Elem (x :: k) (xs :: [k]) :: Bool where
    Elem _ '[]       = 'False
    Elem x (x ': xs) = 'True
    Elem x (y ': xs) = Elem x xs


class i ~ RIndex a as => UElem (a :: k) (as :: [k]) (i :: Nat_) where
  inject :: f a -> NS f as
  match  :: NS f as -> Maybe (f a)

instance UElem a (a ': as) 'Z_ where
  inject x = Z x
  match y = case y of
    Z x -> Just x
    _ -> Nothing

instance ( RIndex a (b ': as) ~ ('S_ i) , UElem a as i) => UElem a (b ': as) ('S_ i) where
  inject x = S (inject x)
  match y = case y of
    Z x -> Nothing
    S y -> match y
    


type family CheckElemIsMember (a :: k) (as :: [k]) :: Constraint where
    CheckElemIsMember a as =
      If (Elem a as) (() :: Constraint) (TypeError (NoElementError a as))
type NoElementError (r :: k) (rs :: [k]) =
          'Text "You require open sum type to contain the following element:"
    ':$$: 'Text "    " ':<>: 'ShowType r
    ':$$: 'Text "However, given list can store elements only of the following types:"
    ':$$: 'Text "    " ':<>: 'ShowType rs

-- | This type family checks whether @a@ is 

