{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module API where

import Control.Monad.Except
import Data.Aeson
import Data.WorldPeace hiding (Nat)
import GHC.Types (Nat)
import Network.HTTP.Types
import Prelude
import Servant.API.Generic
import Servant hiding (Unauthorized)
import Servant.Server


-- | I ended up being in favor of having a dedicated default result distinguished from the
-- exceptions.  I think this will allow us to implement the handlers by *returning* the
-- default result and *throwing* the exceptions.  Also, we always need to have at least one
-- result for the handler to be useful, and with a default result we can allow the exceptions
-- lists to be empty.
--
-- And yes, it's safe!  Bear with me :)
data MultiVerb
  (method :: k)
  (contentTypes :: [*])
  (excepts :: [{- IsWaiError => -} u]) -- ^ This is morally an 'OpenUnion'.
  (resp :: u)  -- ^ 'Resp', or something involving 'ResponseHeader's.

-- | Names are up for debate.
type JVerb method = MultiVerb method '[JSON]

-- | 'Verb' without 'contentTypes' and 'method'; for use in 'MultiVerb'.
data Resp
  (statusCode :: Nat)
  a


data WaiError = WaiError Int String String
  deriving stock (Generic, Eq, Show)

notFound :: WaiError
notFound = WaiError 404 "not-found" "what can i say?  it's not there!"

-- | (In wire, we probably want to define a prefix for all error types, or always imports
-- errors qualified in order to avoid name space cluttering.)
data ENotFound = ENotFound
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

unauthorized :: WaiError
unauthorized = WaiError 403 "unauthorized" "or was it 401?"

data EUnauthorized = EUnauthorized
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)


-- | Helper class for...  hm, not sure?
class IsWaiError err where
  toResp :: err -> WaiError

instance IsWaiError ENotFound where
  toResp _ = notFound

instance IsWaiError EUnauthorized where
  toResp _ = unauthorized


data Routes route = Routes
  { _get :: route :- Capture "id" Int
            :> JVerb 'GET '[ ENotFound
                           , EUnauthorized
                           ]
                           (Resp 200 String)
  , _put :: route :- Capture "id" Int :> ReqBody '[JSON] String
            :> JVerb 'PUT '[ EUnauthorized
                           ]
                           (Resp 200 Bool)
  }
  deriving (Generic)

api :: Proxy (ToServantApi Routes)
api = genericApi @Routes Proxy


-- Server code.  Should go to Server.hs, but it's easier to have it in one module...

class Monad m => MonadMulti (excepts :: [*]) (m :: * -> *) {- result{- not @Resp 200 String@, but @String@! -} -} where
  throwSome :: forall (except :: *) any. IsMember except excepts => except -> m any

instance Monad m => MonadMulti '[except] m where
  throwSome = undefined

instance Monad m => MonadMulti (except ': excepts) m where
  throwSome = undefined


type MultiServer excepts result = Server (Either (OpenUnion excepts) result)

-- | With some luck, this can be called in the 'HasServer' instances and we won't have to see
-- it a lot...
catchSome :: forall excepts m result. MonadMulti excepts m => m result -> MultiServer excepts result
catchSome = undefined


getHandler :: MonadMulti '[ENotFound, EUnauthorized] m => Int -> m String
getHandler i = do
  when (i > 10) $ throwSome EUnauthorized
  when (i `div` 2 /= 0) $ throwSome ENotFound
  pure "12"


putHandler :: MonadMulti '[EUnauthorized] m => Int -> m Bool
putHandler = undefined


-- instance HasServer ...
