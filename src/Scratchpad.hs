{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
-- | "Servant"
{-# OPTIONS_GHC -Wno-unused-imports -Wno-name-shadowing #-}
module Scratchpad where


import Prelude
import Data.String.Conversions
import Data.Kind
import Control.Lens (at, (&), (.~), (?~), (<.), (.>), _Just, set)
import Data.Swagger
import Data.Proxy
import GHC.TypeLits
import Data.Maybe
import Servant hiding (And, Elem)
import Servant.Swagger
import Servant.Swagger.Internal
import Servant.Server.Generic
import Servant.API.ContentTypes
import Servant.API.Generic
import Data.Aeson.Types
import Data.Swagger.Declare

import Data.SOP.NS
import Data.SOP.Constraint
import Data.SOP.BasicFunctors
import           Network.HTTP.Types                         hiding
                 (Header, ResponseHeaders)
import           Network.Wai (Application, requestHeaders, responseLBS)
import           Servant.API.ContentTypes
                 (AcceptHeader (..), AllCTRender (..))

-- TODO I will need these
{-import           Servant.API.ResponseHeaders
                (GetHeaders, Headers, getHeaders, getResponse)
-}


import           Servant.Server.Internal


newtype AsResource status contentTypes headers a = AsResource a


-- TODO: i guess this is so we can have default statusses?  i also guess i'm against it, but i'm not sure.
newtype WithStatus n a = WithStatus a
  deriving newtype (FromJSON, ToJSON)
  deriving stock (Functor)

-- TODO: this typeclass can probably go
class HasStatus a where
  getStatus :: a -> Int  -- TODO: should be @proxy a@, not @a@

instance forall n a. KnownNat n => HasStatus (WithStatus n a) where
  getStatus _ = fromInteger $  natVal (Proxy  @n)

data NotFound = NotFound { msg :: String }
  deriving stock (Generic)
  deriving anyclass (ToJSON, ToSchema)

data UserUnauthorized = UserUnauthorized { msg :: String }
  deriving stock (Generic)
  deriving anyclass (ToJSON, ToSchema)

data UserView = UserView { name :: String }
  deriving stock (Generic)
  deriving anyclass (ToJSON, ToSchema)

data CreateUser = CreateUser { name :: String}
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToSchema)

data UserCreated = UserCreated { name :: String }
  deriving stock (Generic)
  deriving anyclass (ToJSON, ToSchema)


data Verb' (method :: StdMethod) (contentTypes :: [*]) (returns :: [*])


type Get' = Verb' 'GET
type Put' = Verb' 'PUT

data Routes route = Routes
  { get :: route :- Description "gets a user" :> Capture' '[Description "The id to use"] "id" Int
        :> Get' '[JSON] '[ WithStatus 200 UserView , WithStatus 404 NotFound ]
  , put :: route :- Description "Creates a user" :> ReqBody' '[Description "The user you want to create"] '[JSON] CreateUser
        :> Put' '[JSON] '[ WithStatus 201 UserCreated, WithStatus 401 UserUnauthorized ]
  }
  deriving (Generic)



type OpenUnion = NS I


api :: Proxy (ToServantApi Routes)
api = genericApi (Proxy @Routes)

app :: Application
app = genericServe server

-- TODO:
-- We want to have the functional dependency that ret -> status within a handler.
-- so  pureNS ret should infer status. so that we don't need to manually annotate
-- I'm not sure if it's worth it though
--
-- Servant.API.ResponseHeaders does something similar
--

server :: Routes AsServer
server = Routes
  { get = get'
  , put = put'
  }
  where
    get' :: Int -> Handler (OpenUnion '[WithStatus 200 UserView, WithStatus 404 NotFound])
    get' x =
      if even x  -- not found
      then pureNS . WithStatus @404 $ NotFound "Didn't find it"
      else pureNS . WithStatus @200 $ UserView "yo"

    put' ::  CreateUser -> Handler (OpenUnion '[WithStatus 201 UserCreated, WithStatus 401 UserUnauthorized])
    put' (CreateUser name) =
      if False -- unauthorized
      then pureNS . WithStatus @401 $  UserUnauthorized "Nopeee!"
      else pureNS . WithStatus @201 $ UserCreated name


pureNS :: (Applicative f, IsMember x xs) => x -> f (NS I xs)
pureNS = pure . inject'

inject' :: IsMember x xs => x -> NS I xs
inject' = inject . I


injectResourceTypeFactoryBean :: IsMember x xs => Proxy x -> ResourceType x -> NS GetResourceType xs
injectResourceTypeFactoryBean = undefined

-- TODO: Support the 'Headers' newtype wrapper from servant as well
instance (AllMime cts, All (AllCTRender cts `And` HasStatus) returns, ReflectMethod method) => HasServer (Verb' method cts returns) context where
  type ServerT (Verb' method cts returns) m = m (NS I returns)

  hoistServerWithContext _ _ nt s = nt s
  route Proxy _ctx action =  leafRouter route'
    where
      method = reflectMethod (Proxy @method)
      route' env request respond =
        runAction action'  env request respond $ \ (output :: NS I returns) -> do
           let (status, b' :: Maybe (LBS, LBS)) =
                 collapse_NS . cmap_NS
                     (Proxy @(AllCTRender cts `And` HasStatus))
                     (\(I b) -> K (toEnum $ getStatus b, handleAcceptH (Proxy @cts) (AcceptHeader accH) b))
                     $ output
           case b' of
             Nothing -> FailFatal err406 -- this should not happen (checked before), so we make it fatal if it does
             Just (contentT, body) ->
              let bdy = if allowedMethodHead method request then "" else body
              in Route $ responseLBS status ((hContentType, cs contentT) : []) bdy
        where
          accH = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
          action' = action `addMethodCheck` methodCheck method request
                           `addAcceptCheck` acceptCheck (Proxy @cts) accH

type EmptyUnionError =
          'Text "Your endpoint defines no return types, which is an error"

handler :: Server (UVerb 'GET [Resource 201 '[] '[JSON] Bool, Resource 200 '[] '[JSON]  String])
handler = pure $ injectResourceTypeFactoryBean (Proxy @(Resource 201 '[] '[JSON] Bool)) True

data UVerb (method :: StdMethod) (resources :: [k {- Resource -} ])

data Resource (statusCode :: Nat) (headers :: [Symbol]) (contentTypes :: [*]) (return :: *)


{- This is a good idea:
 -
type instance ResourceStatus MyBool = 201
type instance ResourceType MyBool =  400

-- "legalhold" :> LegalHold.API

Oh no maybe not cz errors
-- could not unify x0 ~ ResourceStatus Bool

I like dis more. Ok use typeclass
-- No instance IsResource Bool

-}

class IsResource resource where
  type ResourceStatus       resource :: Nat
--  type ResourceContentTypes resource :: '[*]
  type ResourceType         resource :: *

-- TODO: better name.
-- TODO: (AllMime cts, All (AllCTRender cts `And` HasStatus) returns, ReflectMethod method)
-- type IsGoodResource resource = (IsResource resource, AllMime (ResourceContentTypes resource), ...)


instance IsResource (Resource statusCode headers contentTypes return) where
  type ResourceStatus (Resource statusCode headers contentTypes return)  =  statusCode
--  type ResourceContentTypes resource :: '[*]
  type ResourceType (Resource statusCode headers contentTypes return)  =  return


data GetResourceType :: k -> * where
  MkGetResourceType :: ResourceType resource -> GetResourceType resource


instance {- (All IsGoodResource resource) => -} HasServer (UVerb method resources) context where
  type ServerT (UVerb method resources) m = m (NS GetResourceType resources)

  hoistServerWithContext _ _ nt s = nt s
  route = undefined




-- somehow need to loop through the list of returns ( ? instances with recursion?)
instance (TypeError EmptyUnionError) => HasSwagger (Verb' method cts '[]) where
  toSwagger = undefined

instance
  ( AllAccept cts
  , SwaggerMethod method
  , ToSchema x
  , HasStatus x
  ) => HasSwagger (Verb' method cts (x ': '[])) where
  toSwagger Proxy =
    let
      -- TODO let getStatus take a proxy
      status = getStatus (undefined :: x)
      responseContentTypes = allContentType (Proxy @cts)
      (defs, res) = runDeclare (declareSchemaRef (Proxy @x)) mempty
    in
      mempty & paths.at "/" ?~
        ( mempty & swaggerMethod (Proxy @method) ?~ (mempty
          & produces ?~ MimeList responseContentTypes
          & at status ?~ Inline (mempty & schema ?~ res)
        )) & definitions .~ defs


instance  {-# OVERLAPPABLE #-}
  forall cts method x xs.
  ( AllAccept cts
  , SwaggerMethod method
  , ToSchema x
  , HasStatus x
  , HasSwagger (Verb' method cts xs)
  ) => HasSwagger (Verb' method cts (x ': xs)) where
  toSwagger Proxy =
    let
      -- TODO let getStatus take a proxy
      status = getStatus (undefined :: x)
      responseContentTypes = allContentType (Proxy @cts)
      (defs, res) = runDeclare (declareSchemaRef (Proxy @x)) mempty
    in

      -- TODO: This is not the monoid we think it is. it overrides whatever is on the left with
      -- whatever is on the right. So we're not actually adding multiple status codes
      (mempty & paths.at "/" ?~
        ( mempty & swaggerMethod (Proxy @method) ?~ (mempty
          & produces ?~ MimeList responseContentTypes
          & at status ?~ Inline (mempty & schema ?~ res)
        )) & definitions .~ defs) `mappend` toSwagger (Proxy @(Verb' method cts xs))
      -- Lens magic. idk
      {-setResponseFor (paths.(at "/"). _Just . swaggerMethod (Proxy @method) .  _Just . _)
        status
        (declareResponse (Proxy @x))
        (toSwagger (Proxy @(Verb' method cts xs)))
        -}
    {- setResponseFor (swaggerMethod (Proxy @method)) (getStatus (undefined :: x)) _
     -}

{- some swagger structure for good measure
   - paths:
   -  /:
   -    get:
   -      responses:
   -        200:
   -          content:
   -            application/json:
   -              schema:
   -                $ref: #UserView
   -            application/xml:
   -              schema:
   -                $ref: #UserView
   -        404:
   -          content:
   -            application/json:
   -              schema:
   -                $ref: #NotFound
   -    put:
   -      responses:
   -        201:
   -          content:
   -            application/json:
   -              schema:
   -                $ref: #UserCreated
   -            application/xml:
   -              schema:
   -                $ref: #UserCreated
   -        401:
   -          content:
   -            application/json:
   -              schema:
   -                $ref: #UserUnauthorized
   -
   -}


------------------- Stuff stolen from WorldPeace but for generics-sop

type IsMember (a :: u) (as :: [u]) = (CheckElemIsMember a as, UElem a as (RIndex a as))

type family Contains (as :: [k]) (bs :: [k]) :: Constraint where
  Contains '[] _ = ()
  Contains (a ': as) bs = (IsMember a bs, Contains as bs)

data Nat_ = S_ Nat_ | Z_


-- TODO: we probably can make do without this.
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
    Z _ -> Nothing
    S y -> match y



-- | This type family checks whether @a@ is in list.
type family CheckElemIsMember (a :: k) (as :: [k]) :: Constraint where
    CheckElemIsMember a as =
      If (Elem a as) (() :: Constraint) (TypeError (NoElementError a as))

type NoElementError (r :: k) (rs :: [k]) =
          'Text "Expected one of:"
    ':$$: 'Text "    " ':<>: 'ShowType rs
    ':$$: 'Text "But got:"
    ':$$: 'Text "    " ':<>: 'ShowType r


-- | TODO: steal the Nub list from servant-swagger(?)
