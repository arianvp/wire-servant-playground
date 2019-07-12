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



data NotFound = NotFound 
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data Unauthorized = Unauthorized 
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data Routes route = Routes
  { _get :: route :- Capture "id" Int :> Throws NotFound :> Throws Unauthorized :> Get '[JSON] String
  , _put :: route :- ReqBody '[JSON] Int :> Put '[JSON] Bool
  }
  deriving (Generic)

api :: Proxy (ToServantApi Routes)
api = genericApi @Routes Proxy
