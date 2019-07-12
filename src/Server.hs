module Server where
import API
import Servant.Server.Generic
import Servant.Server

server :: Routes AsServer
server = Routes
  { _get = const _
  , _put = const (return False)
  }

app :: Application
app = genericServe server




