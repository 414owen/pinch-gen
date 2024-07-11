{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE TypeOperators #-}

module Pinch.Gen.Common
  ( APIVersion(..)
  , APIReturn
  , LiftReturn(..)
  , liftWrap
  ) where

import qualified Pinch.Transport as Transport
import Pinch.Internal.RPC (ThriftResult(..))
import Pinch (Pinchable(..))
import Pinch.Internal.TType (TStruct)

data APIVersion 
  = Basic
  | WithHeaders

type family APIReturn (a :: APIVersion) r where
  APIReturn 'WithHeaders r = (r, Transport.HeaderData)
  APIReturn 'Basic r = r

class ThriftResult r' => LiftReturn (apiVersion :: APIVersion) r r' where
  liftReturn :: r -> (r', Transport.HeaderData)

instance ThriftResult r => LiftReturn 'WithHeaders r r where
  liftReturn  = (, Transport.emptyHeaderData)

instance ThriftResult r => LiftReturn 'Basic (r, Transport.HeaderData) r where
  liftReturn  = Prelude.id

newtype PinchHeaderWrapper a = PinchHeaderWrapper (a, Transport.HeaderData)

instance Pinchable a => Pinchable (PinchHeaderWrapper a) where
  type Tag (PinchHeaderWrapper a) = Tag a
  pinch (PinchHeaderWrapper (a, _)) = pinch a
  unpinch a = PinchHeaderWrapper . (, Transport.emptyHeaderData) <$> unpinch a
  
instance ThriftResult a => ThriftResult (PinchHeaderWrapper a) where
  type ResultType (PinchHeaderWrapper a) = ResultType a
  wrap m = PinchHeaderWrapper . (, Transport.emptyHeaderData) <$> wrap m
  unwrap (PinchHeaderWrapper (a, _)) = unwrap a

-- wrap :: IO (ResultType a) -> IO a
-- liftReturn :: r -> (r', Transport.HeaderData)

liftWrap :: forall (apiVersion :: APIVersion) a.
  (ThriftResult (APIReturn apiVersion a), LiftReturn apiVersion (APIReturn apiVersion a) a)
  => IO (ResultType (APIReturn apiVersion a)) -> IO (a, Transport.HeaderData)
liftWrap act = liftReturn @apiVersion <$> wrap @(APIReturn apiVersion a) act
