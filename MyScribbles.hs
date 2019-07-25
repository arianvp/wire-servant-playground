



data OK typs a = OK a
data Created types a = Created a
data NoContent types a = NoContent
data Unauthorized types a = Unauthorized a
data Forbidden types a = Forbidden a


data UserCreated = User String deriving (ToJSON)
data UserView = User String deriving (ToJSON)

data NoContent'

data UnauthorizedMsg = UnauthorizedMsg String deriving (ToJSON, ToXML)


-- A route that can have multiple return types
data VerbOf method rets

data Ret ret rets


instance All HasServer rets => HasServer (Rets rets) where
  type (Rets rets) = OpenUnion ...

instance Member ret rets => HasServer (Ret ret rets) where


-- need to write an instance Such that
Server (VerbOf POST [Created '[JSON] User, Unauthorized '[JSON] UnauthorizedMsg])
 = IO (OpenUnion [UserCreated, UnauthorizedMsg])

Server (VerbOf PUT '[NoContent, Forbidden '[JSON] ForbiddenMsg])
 = IO (OpenUnion [NoContent, ForbiddenMsg])


handlePost ::  Server (VerbOf POST [Created '[JSON] User, Unauthorized '[JSON] UnauthorizedMsg])
handlePost =
  if authorized
  then return (openUnionLift (UserCreated "Ben"))
  else return (openUnionLift (UnauthorizedMsg "Authentication failed my dear"))

handlePut :: Server (VerbOf PUT '[NoContent, Forbidden '[JSON] ForbiddenMsg])
handlePut =
  if internalEndpoint
  then return (openUnionLift (ForbiddenMsg "Thou Shalt not pass"))
  else return (openUnionLift NoContent)

handler :: IO (OpenUnion [Ok User, Unauthorized UnauthorizedMsg ])
handler = do
  if False
  then return (openUnionLift (Unauthorized (UnauthorizedMsg "nooo")))
  else return (openUnionLift (OK (User "Gerrit"))


