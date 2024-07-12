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
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ConstraintKinds #-}

module Pinch.Gen.Common
  ( APIVersion(..)
  , APIReturn
  , LiftWrap
  , liftWrap
  ) where

import qualified Pinch.Transport as Transport
import Pinch.Internal.RPC (ThriftResult(..))
import Control.Arrow (first)

data APIVersion
  = Basic
  | WithHeaders

type family APIReturn (a :: APIVersion) r where
  APIReturn 'WithHeaders r = (r, Transport.HeaderData)
  APIReturn 'Basic r = r

class ToHeadered (apiVersion :: APIVersion) a b where
  toHeadered :: a -> (b, Transport.HeaderData)

instance ToHeadered 'WithHeaders (r, Transport.HeaderData) r where
  toHeadered = id

instance ToHeadered 'Basic r r where
  toHeadered = (, Transport.emptyHeaderData)

{-
class ThriftResult r => LiftReturn (apiVersion :: APIVersion) r where
  liftReturn :: IO (ResultType r) -> IO (r, Transport.HeaderData)

instance ThriftResult r => LiftReturn 'WithHeaders r where
  liftReturn act = (, Transport.emptyHeaderData) <$> wrap act

instance ThriftResult r => LiftReturn 'Basic (r, Transport.HeaderData) where
  liftReturn = wrap

newtype PinchHeaderWrapper a = PinchHeaderWrapper (a, Transport.HeaderData)
-}

{-
instance Pinchable a => Pinchable (a, Transport.HeaderData) where
  type Tag (a, _) = Tag a
  pinch (a, _) = pinch a
  unpinch a = (, Transport.emptyHeaderData) <$> unpinch a

instance ThriftResult a => ThriftResult (a, Transport.HeaderData) where
  type ResultType (a, _) = (ResultType a, Transport.HeaderData)
  wrapThrown m = wrapThrown $ fst <$> m
  unwrap (a, headers) = (, headers) <$> unwrap a
-}

type LiftWrap apiVersion a = 
  ( ToHeadered apiVersion (APIReturn apiVersion (ResultType a)) (ResultType a)
  , ToHeadered apiVersion (APIReturn apiVersion a) a
  , ThriftResult (APIReturn apiVersion a)
  , ThriftResult a
  )

liftWrap
  :: forall (apiVersion :: APIVersion) a.
  ( ToHeadered apiVersion (APIReturn apiVersion (ResultType a)) (ResultType a)
  , ToHeadered apiVersion (APIReturn apiVersion a) a
  , ThriftResult (APIReturn apiVersion a)
  , ThriftResult a
  )
  => IO (APIReturn apiVersion (ResultType a))
  -> IO (a, Transport.HeaderData)
liftWrap act = flip fmap (wrapThrown act) $ \case
  Left err -> toHeadered @apiVersion (err :: APIReturn apiVersion a)
  Right a -> first wrapPure $ toHeadered @apiVersion a

{-
instance Pinchable a => Pinchable (PinchHeaderWrapper a) where
  type Tag (PinchHeaderWrapper a) = Tag a
  pinch (PinchHeaderWrapper (a, _)) = pinch a
  unpinch a = PinchHeaderWrapper . (, Transport.emptyHeaderData) <$> unpinch a
  
instance ThriftResult a => ThriftResult (PinchHeaderWrapper a) where
  type ResultType (PinchHeaderWrapper a) = ResultType a
  wrap m = PinchHeaderWrapper . (, Transport.emptyHeaderData) <$> wrap m
  unwrap (PinchHeaderWrapper (a, _)) = unwrap a
-}

-- wrap :: IO (ResultType a) -> IO a
-- liftReturn :: r -> (r', Transport.HeaderData)

-- liftWrap :: forall (apiVersion :: APIVersion) a.
--   (ThriftResult (APIReturn apiVersion a), LiftReturn apiVersion (APIReturn apiVersion a) a)
--   => IO (ResultType (APIReturn apiVersion a)) -> IO (a, Transport.HeaderData)
-- liftWrap act = liftReturn @apiVersion <$> wrap @(APIReturn apiVersion a) act
