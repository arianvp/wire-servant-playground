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
import GHC.TypeLits
import Network.HTTP.Types
import Prelude
import Servant.API.Generic
import Servant hiding (Unauthorized)
import Servant.Server


----------------------------------------------------------------------
-- | multi-verbs.

data MultiVerb
  (method :: k)
  (resps :: [u]) -- ^ This is morally an 'OpenUnion'.

-- | 'Verb' without 'contentTypes' and 'method'; for use in 'MultiVerb'.
data Resp
  (statusCode :: Nat)
  (contentTypes :: [*])
  a

-- | Special case that will save us a lot of boiler plate: errors are always json bodies of a
-- very specific type.
data ErrorResp
  (statusCode :: Nat)
  (label :: Symbol)
  (message :: Symbol)

type ENotFound = ErrorResp 404 "not-found" "what can i say?  it's not there!"
type EUnauthorized = ErrorResp 403 "unauthorized" "or was it 401?"


----------------------------------------------------------------------
-- reducing everything to the 'Resp' type may save us the trouble of writing more 'HasServer'
-- instances.

{-

class IsResp resp where
  type family ToResp resp :: Resp statusCode contentTypes a

instance IsResp Resp where
  type ToResp (Resp statusCode contentTypes a) = (Resp statusCode contentTypes a)

instance IsResp (ErrorResp status label message) where
  type ToResp = ...

-}


----------------------------------------------------------------------
-- routing table

data Routes route = Routes
  { _get :: route :- Capture "id" Int
            :> MultiVerb 'GET '[ Resp 200 '[JSON] String
                               , ENotFound
                               , EUnauthorized
                               ]
  , _put :: route :- Capture "id" Int :> ReqBody '[JSON] String
            :> MultiVerb 'PUT '[ Resp 200 '[JSON] Bool
                               , EUnauthorized
                               ]
  }
  deriving (Generic)

api :: Proxy (ToServantApi Routes)
api = genericApi @Routes Proxy


----------------------------------------------------------------------
-- Server code.

-- (Should go to Server.hs, but it's easier to have it in one module...)

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
getHandler _i = do
--  when (i > 10) $ throwSome EUnauthorized
--  when (i `div` 2 /= 0) $ throwSome ENotFound
  pure "12"


putHandler :: MonadMulti '[EUnauthorized] m => Int -> m Bool
putHandler = undefined


-- instance HasServer ...


----------------------------------------------------------------------
-- experiment with exceptions

{-

coolThrow :: forall m e es any. (IsMember e es, MonadError (OpenUnion es) m) => e -> m any
coolThrow = throwError . openUnionLift

coolPure :: (Monad m, IsMember e es) => e -> m (OpenUnion es)
coolPure = pure . openUnionLift

coolCatch :: forall (m :: * -> *) (e :: *) (es :: [*]). (Contains es (e : es), Functor m) => ExceptT (OpenUnion es) m e -> m (OpenUnion (e : es))
coolCatch = fmap (either relaxOpenUnion openUnionLift) . runExceptT

coolDemonstration :: IO (OpenUnion '[Int, Bool, String])
coolDemonstration = coolCatch $ do
  when False $ coolThrow (3 :: Int)
  when True $ coolThrow False
  coolPure ("bla" :: String)



[1 of 1] Compiling API              ( API.hs, interpreted )

API.hs:139:16: warning: [-Wdeferred-type-errors]
    • You require open sum type to contain the following element:
          Int
      However, given list can store elements only of the following types:
          '[Bool, String]
    • In the second argument of ‘($)’, namely ‘coolThrow (3 :: Int)’
      In a stmt of a 'do' block: when False $ coolThrow (3 :: Int)
      In the second argument of ‘($)’, namely
        ‘do when False $ coolThrow (3 :: Int)
            when True $ coolThrow False
            coolPure ("bla" :: String)’
    |
139 |   when False $ coolThrow (3 :: Int)
    |                ^^^^^^^^^^^^^^^^^^^^

API.hs:141:3: warning: [-Wdeferred-type-errors]
    • You require open sum type to contain the following element:
          String
      However, given list can store elements only of the following types:
          es0
    • In a stmt of a 'do' block: coolPure ("bla" :: String)
      In the second argument of ‘($)’, namely
        ‘do when False $ coolThrow (3 :: Int)
            when True $ coolThrow False
            coolPure ("bla" :: String)’
      In the expression:
        coolCatch
          $ do when False $ coolThrow (3 :: Int)
               when True $ coolThrow False
               coolPure ("bla" :: String)
    |
141 |   coolPure ("bla" :: String)
    |   ^^^^^^^^^^^^^^^^^^^^^^^^^^
-}
