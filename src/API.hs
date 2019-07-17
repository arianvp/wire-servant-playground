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
import GHC.TypeLits
import GHC.Types (Nat)
import Network.HTTP.Types
import Prelude
import Servant.API.Generic
import Servant hiding (Unauthorized)
import Servant.Server
import Servant.Server.Generic


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

server :: Routes AsServer
server = Routes
  { _get = getHandler
  , _put = putHandler
  }

app :: Application
app = genericServe server


getHandler :: Int -> Server (MultiVerb 'GET '[ Resp 200 '[JSON] String
                                             , ENotFound
                                             , EUnauthorized
                                             ])
-- use @Handler (OpenUnion ...)@ instead of @Server (MultiVerb ...@

-- another fun puzzle: since the status codes are provided on the type level, we will have to
-- match the actual value returned by the handler with the entry in the union type where that
-- status code is given (implicitly in the case or 'Resp' and in the 'IsResp' instance in the
-- case of 'ErrorResp').

getHandler = undefined

putHandler :: Int -> String -> Server (MultiVerb 'PUT '[ Resp 200 '[JSON] Bool
                                                       , EUnauthorized
                                                       ])
putHandler = undefined





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
