module Server where
import API
import Servant.Server.Generic
import Servant.Server
import Servant.Checked.Exceptions

server :: Routes AsServer
server = Routes
  { _get = const (pureSuccEnvelope "yo")
  , _put = const (return False)
  }

app :: Application
app = genericServe server




