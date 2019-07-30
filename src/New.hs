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


this is actually quite close to 'Verb' in its latest shape and flavor.  It's like having a
ToJSON instance for an open union type, with the (important!) difference that the status code
is determined by the specific element in the union.


TODO: outdated!!  merge this with the haddocks for 'UVerb'!

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

{- | Variant of the 'Verb' type for handlers that return an open union of different types (eg.,
@'[Thing, Error]@).  'UVerb' defines request method, supported response body encodings, and a
list containing all responses types that the implementing handler can return.

@mkres@ is a @newtype@ wrapper that needs to be introduced by the library user.  It lets you
write 'HasStatusCode' instances @mkres t@ for any application type @t@ without introducing
orphans, for as many @t@ as you like at the cost of a single @newtype@.  See 'MakesResource'.

The set of supported content types is defined once for all items in the open union of return
types.


**Keep reading if you are interested in the design choicse we made.**

it would be nice to have individual content types supposed for different items in the union,
something like

>>> data UVerb (mkres :: * -> *) (method :: StdMethod) (resources :: [*])
>>>
>>> class IsResource (resource :: *) where
>>>   type resourceContentTypes resource :: [*]

This has two bad implementations: (a) content negotiation happens with 'addAcceptCheck' before
the handler is called.  Then we don't know what the supported response content types will be,
because that depends on what item of the union the handler will return later.  We could
compute the intersection of all items in the union and process the accept header based on
that, but that seems complicated and weird.  (b) content negotiation happens *after* the
handler is called, but 'fmap'-ping a function @:: Route a -> Route a@ into the 'Delayed'
response.  Then the handler would be called with all the effects it may have (touching the
database, sending out emails, ...) by the time the response is @406 bad Accept header@.  This
is at best confusing.

-}
data UVerb (mkres :: * -> *) (method :: StdMethod) (cts :: [*]) (resources :: [*])

-- | 'getStatus' takes a proxy, because some library (eg., swagger, client) have no resource
-- values to pass in here.
class HasStatus (mkres :: * -> *) (resource :: *) where
  getStatus :: forall (proxy :: * -> *). proxy (mkres resource) -> Status

-- | Used in the 'UVerb' has 'HasServer' instance.
type IsResource cts mkres =
  ( Compose (AllCTRender cts) mkres `And`
    HasStatus mkres `And`
    MakesResource mkres
  )

instance
  ( ReflectMethod method
  , AllMime cts
  , All (IsResource cts mkres) resources
  ) => HasServer (UVerb mkres method cts resources) context where
  type ServerT (UVerb mkres method cts resources) m = m (NS mkres resources)

  hoistServerWithContext _ _ nt s = nt s
  route Proxy _ctx action = leafRouter route'
    where
      method = reflectMethod (Proxy @method)

      route' env request respond = do
        let accH = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
            action' = action
                `addMethodCheck` methodCheck method request
                `addAcceptCheck` acceptCheck (Proxy @cts) accH

            mkProxy :: a -> Proxy a
            mkProxy _ = Proxy

        runAction action' env request respond $ \(output :: NS mkres resources) -> do
          let encodeResource
                :: forall resource.
                   IsResource cts mkres resource =>
                   mkres resource -> K (Status, Maybe (LBS, LBS)) resource
              encodeResource res = K
                  ( getStatus $ mkProxy res
                  , handleAcceptH (Proxy @cts) (AcceptHeader accH) res
                  )

              pickResource
                :: NS mkres resources -> (Status, Maybe (LBS, LBS))
              pickResource = collapse_NS . cmap_NS (Proxy @(IsResource cts mkres)) encodeResource

          case pickResource output of
            (_, Nothing) -> FailFatal err406 -- this should not happen (checked before), so we make it fatal if it does
            (status, Just (contentT, body)) ->
              let bdy = if allowedMethodHead method request then "" else body
              in Route $ responseLBS status ((hContentType, cs contentT) : []) bdy

-- | 'return' for 'UVerb' handlers.  Pass it a value of an application type from the routing
-- table, and it will return a value of the union of responses.
respond
  :: forall (f :: * -> *) (mkres :: * -> *) (x :: *) (xs :: [*]).
     (Applicative f, MakesResource mkres x, IsMember x xs)
  => x -> f (NS mkres xs)
respond = pure . inject . mkResource

class MakesResource (mkres :: * -> *) (value :: *) where
  mkResource :: value -> mkres value


-- * example (code using the library)

-- | Application-specific resource marker.  By wrapping application types defined in other
-- modules with this, we can write non-orphan 'HasStatus' instances.
--
-- TODO: get rid of this?
newtype Resource (value :: *) = Resource (value :: *)
  deriving newtype (Eq, Show, Generic, FromJSON, ToJSON {-, ToSchema -- see issue in swagger2 -})

instance MakesResource Resource value where
  mkResource = Resource

-- ...  now you want to define a bunch of shortcuts for UVerb that suit your api best.

type API = UVerb Resource 'GET '[JSON] [Bool, String]

instance HasStatus Resource Bool   where getStatus _ = status201
instance HasStatus Resource String where getStatus _ = status303

handler :: Server API
handler = if False then respond ("True" :: String) else respond True

app :: Application
app = serve (Proxy @API) handler


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
