{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}

module Pinch.Gen.Common
  ( APIVersion(..)
  , APIReturn(..)
  , liftWrap
  ) where

import qualified Pinch.Transport as Transport
import Pinch.Internal.RPC (ThriftResult(..))

data APIVersion 
  = Basic
  | WithHeaders

type family APIReturn (a :: APIVersion) r where
  APIReturn 'WithHeaders r = (r, Transport.HeaderData)
  APIReturn 'Basic r = r

class LiftReturn (apiVersion :: APIVersion) r r' where
  liftReturn :: r -> (r', Transport.HeaderData)

instance LiftReturn 'WithHeaders r r where
  liftReturn  = (, Transport.emptyHeaderData)

instance LiftReturn 'Basic (r, Transport.HeaderData) r where
  liftReturn  = Prelude.id

liftWrap :: forall (apiVersion :: APIVersion) a. (LiftReturn apiVersion a a, ThriftResult a) => IO (ResultType a) -> IO (a, Transport.HeaderData)
liftWrap act = liftReturn @apiVersion <$> wrap @a act

