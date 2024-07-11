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
{-# LANGUAGE LambdaCase #-}

module Pinch.Gen.Common
  ( APIVersion(..)
  , APIReturn
  , liftReturn
  ) where

import qualified Pinch.Transport as Transport
import Pinch.Internal.RPC (ThriftResult(..))

data APIVersion
  = Basic
  | WithHeaders

type family APIReturn (a :: APIVersion) r where
  APIReturn 'WithHeaders r = (ResultType r, Transport.HeaderData)
  APIReturn 'Basic r = ResultType r

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

instance Pinchable a => Pinchable (a, Transport.HeaderData) where
  type Tag (a, _) = Tag a
  pinch (a, _) = pinch a
  unpinch a = (, Transport.emptyHeaderData) <$> unpinch a

instance ThriftResult a => ThriftResult (a, Transport.HeaderData) where
  type ResultType (a, _) = (ResultType a, Transport.HeaderData)
  wrap m = do
    m' <- once m
    res <- wrap $ fst <$> m'
    (_, headerData) <- m'
    pure (res, headerData)
  unwrap (a, headers) = (, headers) <$> unwrap a
-}

liftReturn :: forall (apiVersion :: APIVersion) a.
  ( ThriftResult (a, Transport.HeaderData)
  , ToHeadered apiVersion (APIReturn apiVersion (ResultType a)) a
  )
  => IO (APIReturn apiVersion (ResultType a))
  -> IO (a, Transport.HeaderData)
liftReturn act = flip fmap (wrapThrown act) $ \case
  Left err -> err
  Right a -> toHeadered @apiVersion a

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
