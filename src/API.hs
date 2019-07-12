{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
module API where

import Servant hiding (Unauthorized)
import Servant.API.Generic
import Servant.Checked.Exceptions


data ErrorType = NotFound | Unauthorized

data Error (a :: ErrorType) = Error { _label :: String, _Body :: String }

data Routes route = Routes
  { _get :: route :- Capture "id" Int :> Throws (Error 'NotFound) :> Throws (Error 'Unauthorized) :> Get '[JSON] String
  , _put :: route :- ReqBody '[JSON] Int :> Put '[JSON] Bool
  }
  deriving (Generic)

api :: Proxy (ToServantApi Routes)
api = genericApi @Routes Proxy
