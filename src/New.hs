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
import           Data.String.Conversions
                 (cs)
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
import           Servant.API.ResponseHeaders
                 (GetHeaders, Headers, getHeaders, getResponse)
import           Servant.Server.Internal



-- * the new stuff (library)

type OpenUnion = NS Resource  -- TODO: 'Resource' is part of the application, not the library.
                              -- also, we probably want to use @NS f@ instead of @OpenUnion@.

data UVerb (method :: StdMethod) (resources :: [*])

class IsResource (resource :: *) where
  type ResourceStatus       resource :: Nat
  type ResourceHeaders      resource :: [Symbol]
  type ResourceContentTypes resource :: [*]

instance {- TODO: (AllMime cts, All (AllCTRender cts `And` HasStatus) returns, ReflectMethod method) => -}
         (All IsResource resources) =>
         HasServer (UVerb (method :: StdMethod) (resources :: [*])) context where
  type ServerT (UVerb method resources) m = m (OpenUnion resources)

  hoistServerWithContext _ _ nt s = nt s
  route = undefined


-- * example (code using the library)

-- | Application-specific resource marker.  By wrapping application types defined in other
-- modules with this, we can write non-orphan 'IsResource' instances.
newtype Resource (value :: *) = Resource (value :: *)
  deriving newtype (Eq, Show, Generic)

-- | We also need to construct a variant of 'pure' that takes as much of the boilerplate out
-- of the handlers as possible.  this needs to be defined in the app, since it makes use of
-- the app-specific 'Resource' type.
--
-- Copy the type signature from 'mkRespond', and specialize.
--
-- TODO: add a class to the library that forces us to instantiate respond for @resource :: *
-- -> *@ and make it easy.
respond
  :: forall (x :: *) (xs :: [*]).
     (IsMember x xs) =>
     x -> Handler (OpenUnion xs)
respond = pure . inject . Resource


-- | TODO: we can probably hide the 'Resource' here, and simply write @[Bool, String]@.
type API = UVerb 'GET [ Bool
                      , String
                      ]

instance IsResource (Resource Bool) where
  type ResourceStatus       (Resource Bool) = 201
  type ResourceHeaders      (Resource Bool) = '[]
  type ResourceContentTypes (Resource Bool) = '[JSON]

instance IsResource (Resource String) where
  type ResourceStatus       (Resource String) = 303
  type ResourceHeaders      (Resource String) = '["Location"]
  type ResourceContentTypes (Resource String) = '[PlainText, JSON]


handler :: Server API
handler = respond True


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
