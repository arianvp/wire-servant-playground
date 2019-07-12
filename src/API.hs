{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module API where

import Servant hiding (Unauthorized)
import Servant.API.Generic
import Servant.Checked.Exceptions
import Data.Aeson.Types
import Network.HTTP.Types



data NotFound = NotFound 
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

instance ErrStatus NotFound where
  toErrStatus = const status404

data Unauthorized = Unauthorized 
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

instance ErrStatus Unauthorized where
  toErrStatus = const status401


data Routes route = Routes
  { _get :: route :- Capture "id" Int :> Throws NotFound :> Throws Unauthorized :> Get '[JSON] String
  , _put :: route :- ReqBody '[JSON] Int :> Put '[JSON] Bool
  }
  deriving (Generic)

api :: Proxy (ToServantApi Routes)
api = genericApi @Routes Proxy
