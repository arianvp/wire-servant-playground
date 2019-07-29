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

{- | ...

** Design rationales

We want to be able to return an arbitrary application type (or a union of types) from the
handler, and the api type should give servant enough information to construct the response
from that, namely status code, headers (this library provides an alternative to 'Headers' for
slightly nicer syntax; see below), and allowed content types.  If we do that in the form of

@
type AsResource status headers ctyps body = AsResource body
@

@servant@ won't be able to infer the phantom types, and we will need to provide either 'Proxy'
parameters or work with @-XTypeApplications@.  Both choices make writing handlers more awkward
for the user of this library.  ('Headers' does not have this problem, since other than for the
status code, the handler needs to provide header values, which can guide type inference.)

So instead, we introduce a type class @IsResource, and the instance for type @X@ enriches
values of type @X@ with the extra response information:

...


** Headers in @servant*@ vs. @servant-uverb@

...

-}
module New where


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
import Network.Wai.Internal (Request, Response)

import Data.SOP.NS
import Data.SOP.Constraint
import Data.SOP.BasicFunctors
import           Network.HTTP.Types                         hiding
                 (Header, ResponseHeaders)
import           Network.Wai (Application, requestHeaders, responseLBS)
import           Servant.API.ContentTypes
                 (AcceptHeader (..), AllCTRender (..))
import           Servant.API.ResponseHeaders
                 (GetHeaders, Headers, getHeaders, getResponse)
import           Servant.Server.Internal



-- * the new stuff (library)

data UVerb (mkres :: * -> *) (method :: StdMethod) (resources :: [*])

class IsResource (resource :: *) where
  type ResourceStatus       resource :: Nat
  type ResourceHeaders      resource :: [Symbol]
  type ResourceContentTypes resource :: [*]


-- FUTUREWORK: go back to the nice collapse_NS, map_NS design?


instance {-# OVERLAPPABLE #-}
  ( All IsResource (resource ': resource' ': resources)
  , ReflectMethod method
  , AllCTRender (ResourceContentTypes resource) resource
  , AllMime (ResourceContentTypes resource)  -- implied, but i don't care!
  , mkres ~ Resource
  , KnownNat (ResourceStatus resource)
  , HasServer (UVerb mkres method (resource' ': resources)) ctx
  -- TODO: move some constraints to IsResource class?
  ) => HasServer (UVerb mkres method (resource ': resource' ': resources)) ctx where
  type ServerT (UVerb mkres method (resource ': resource' ': resources)) m = m (NS mkres (resource ': resource' ': resources))

  hoistServerWithContext _ _ nt s = nt s
  route Proxy _ctx (action :: Delayed env (Handler (NS mkres (_resource : resource' : resources)))) = leafRouter route'
    where
      method = reflectMethod (Proxy @method)

      route' env request respond = do
        let action' :: Delayed env (Handler a00)
            action' = buildAction action accH

            accH :: SBS
            accH = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request

        runAction action' env request respond (buildAction method env request)



-- class MakesResponse _ _ where
--   mkResponse :: _


buildAction :: forall env resource resource' resources handler handler'.
  ( handler ~ Delayed env (Handler (NS Resource (resource : resource' : resources)))
  , handler' ~ Delayed env (Handler (NS Resource (resource' : resources)))
  ) =>
  handler -> SBS -> handler'
buildAction = undefined
{-
mkAction' :: _
mkAction' action accH = action
  `addMethodCheck` methodCheck method request
  `addAcceptCheck` acceptCheck (Proxy @(ResourceContentTypes resource))
  accH

-}

buildActionRunner :: Method -> env -> Request -> (NS Resource (x ': xs)) -> RouteResult Network.Wai.Internal.Response
buildActionRunner _ _ _ _ = undefined
        {-\case
         (Z (Resource (output :: resource))) -> do
          case handleAcceptH (Proxy @(ResourceContentTypes resource)) (AcceptHeader accH) output of
            Nothing -> FailFatal err406 -- this should not happen (checked before), so we make it fatal if it does
            Just (_contentT, body) ->
              let bdy = if allowedMethodHead method request then "" else body
              in Route $ responseLBS status ((hContentType, undefined {- cs contentT -}) : []) bdy

            status = toEnum . fromInteger $ natVal (Proxy @(ResourceStatus resource))



our solution will do the accept header check on the request after running the handler.  this
is necessary, because the handler decides which type to return, which decides which content
types are supported.  it is also bad, since the handler may have caused effects before servant
responds with 406.
solution: content types are given on UVerb level, not on IsResource level.


headers will work with the old servant approach.  re-think that later, independently!


IsResource will be renamed to HasStatus


-}



instance {-# OVERLAPPING #-}
  ( All IsResource '[resource]
  , ReflectMethod method
  , AllCTRender (ResourceContentTypes resource) resource
  , AllMime (ResourceContentTypes resource)  -- implied, but i don't care!
  , mkres ~ Resource
  , KnownNat (ResourceStatus resource)
  -- TODO: move some constraints to IsResource class?
  ) => HasServer (UVerb (mkres :: * -> *) (method :: StdMethod) ('[resource])) context where
  type ServerT (UVerb mkres method '[resource]) m = m (NS mkres '[resource])

  hoistServerWithContext _ _ nt s = nt s
  route Proxy _ctx (action :: Delayed env (Handler (NS mkres '[resource]))) = leafRouter route'
    where
      method = reflectMethod (Proxy @method)

      route' env request respond = do
        let action' = (action `addMethodCheck` methodCheck method request) `addAcceptCheck` acceptCheck (Proxy @(ResourceContentTypes resource)) accH
            accH = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
            status = toEnum . fromInteger $ natVal (Proxy @(ResourceStatus resource))

        runAction action' env request respond $ \(Z (Resource (output :: resource))) -> do
          case handleAcceptH (Proxy @(ResourceContentTypes resource)) (AcceptHeader accH) output of
            Nothing -> FailFatal err406 -- this should not happen (checked before), so we make it fatal if it does
            Just (_contentT, body) ->
              let bdy = if allowedMethodHead method request then "" else body
              in Route $ responseLBS status ((hContentType, undefined {- cs contentT -}) : []) bdy

instance {-# OVERLAPPING #-}
  ( All IsResource '[]
  , TypeError ('Text "Your endpoint defines no return types, which is an error")
  ) => HasServer (UVerb mkres method '[]) ctx where
  type ServerT (UVerb mkres method '[]) m = m ()
  hoistServerWithContext _ _ _ = undefined
  route = undefined


-- | 'return' for 'UVerb' handlers.  Pass it a value of an application type from the routing
-- table, and it will return a value of the union of responses.
respond
  :: forall (f :: * -> *) {- (mkres :: * -> *) -} (x :: *) (xs :: [*]).
     (Applicative f, {- MakesResource mkres x, -} IsMember x xs)
  => x -> f (NS Resource xs)
respond = pure . inject . Resource

-- | TODO: headers will complicate this again somewhat.  but let's touch this when everything
-- else works.
class MakesResource (mkres :: * -> *) (value :: *) where
  mkResource :: value -> mkres value


-- * example (code using the library)

-- | Application-specific resource marker.  By wrapping application types defined in other
-- modules with this, we can write non-orphan 'IsResource' instances.
--
-- TODO: get rid of this?
newtype Resource (value :: *) = Resource (value :: *)
  deriving newtype (Eq, Show, Generic)

instance MakesResource Resource value where
  mkResource = Resource

-- ...  now you want to define a bunch of shortcuts for UVerb that suit your api best.

type API = UVerb Resource 'GET [Bool, String]

instance IsResource (Resource Bool) where
  type ResourceStatus       (Resource Bool) = 201
  type ResourceHeaders      (Resource Bool) = '[]
  type ResourceContentTypes (Resource Bool) = '[JSON]

instance IsResource (Resource String) where
  type ResourceStatus       (Resource String) = 303
  type ResourceHeaders      (Resource String) = '["Location"]
  type ResourceContentTypes (Resource String) = '[PlainText, JSON]


handler :: Server API
handler = if True then respond ("True" :: String) else respond True


-- * old stuff, helpers.

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
