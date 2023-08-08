{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module System.Environment.FromEnv
(
  -- * Core class
    FromEnv (..)
  -- * Options
  , defaultEnvOpts
  , FromEnvOptions ( optsFieldLabelModifier )
  -- * Generic parsing class
  , GFromEnv (..)
  -- * Errors
  , FromEnvError (..)
) where

import           Control.Applicative                 (liftA2)
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Data.List                           (intercalate)
import           GHC.Generics
import           System.Environment                  (lookupEnv)

import           Text.Casing                         (screamingSnake)

import           System.Environment.FromEnv.TryParse


-- | Class for things that can be created from environment variables.
class FromEnv a where
  fromEnv :: (MonadIO m) => m (Either FromEnvError a)
  default fromEnv :: (MonadIO m, Generic a, GFromEnv' (Rep a)) => m (Either FromEnvError a)
  fromEnv = gFromEnv defaultEnvOpts

instance (FromEnv a, FromEnv b) => FromEnv (a, b) where
  fromEnv = do
    t <- (,) <$> fromEnv <*> fromEnv
    return $ case t of
      (Left e1, Left e2) -> Left $ AggregateError [e1, e2]
      (Left e, Right _)  -> Left e
      (Right _, Left e)  -> Left e
      (Right a, Right b) -> Right (a, b)

instance (FromEnv a, FromEnv b, FromEnv c) => FromEnv (a, b, c) where
  fromEnv = do
    t <- (,,) <$> fromEnv <*> fromEnv <*> fromEnv
    return $ case t of
      (Left e1, Left e2, Left e3)  -> Left $ AggregateError [e1, e2, e3]
      (Left e1, Left e2, Right  _) -> Left $ AggregateError [e1, e2]
      (Right _, Left e1, Left e2)  -> Left $ AggregateError [e1, e2]
      (Left e1, Right _, Left e2)  -> Left $ AggregateError [e1, e2]
      (Left e, Right _, Right _)   -> Left e
      (Right _, Left e, Right _)   -> Left e
      (Right _, Right _, Left e)   -> Left e
      (Right a, Right b, Right c)  -> Right (a, b, c)

-- | Try to convert a field name into an environment variable name.
type FieldLabelModifier = String -> String

-- | Options to specify how to construct your datatype from environment variables.
-- Options can be set using record update syntax and 'defaultEnvOpts'.
newtype FromEnvOptions = FromEnvOptions
  { optsFieldLabelModifier :: FieldLabelModifier
  -- ^ Function to map from a field name to an environment variable name.
  }

-- | Default 'FromEnvOptions':
--
-- The default options will try to read a field name fieldName from an
-- environment variables FIELD_NAME, as this is the most common naming
-- convention for environment variables.
--
-- If you want different behavior, see 'gFromEnv'.
--
-- @
-- 'FromEnvOptions'
-- { 'optsFieldLabelModifier' = Just . 'Text.Casing.screamingSnake'
-- }
-- @
defaultEnvOpts :: FromEnvOptions
defaultEnvOpts = FromEnvOptions
  { optsFieldLabelModifier =  screamingSnake
  }

class GFromEnv a where
  -- | Try to construct a value from environment variables.
  gFromEnv :: (MonadIO m) => FromEnvOptions -> m (Either FromEnvError a)
  default gFromEnv :: (MonadIO m, Generic a, GFromEnv' (Rep a)) => FromEnvOptions -> m (Either FromEnvError a)
  gFromEnv opts = fmap to <$> gFromEnv' opts

instance (Generic a, GFromEnv' (Rep a)) => GFromEnv a

class GFromEnv' f where
  gFromEnv' :: (MonadIO m) => FromEnvOptions -> m (Either FromEnvError (f a))

instance {-# OVERLAPPING #-} GFromEnv' f => GFromEnv' (M1 i c f) where
  gFromEnv' converter = fmap M1 <$> gFromEnv' converter

instance (GFromEnv' f, GFromEnv' g) => GFromEnv' (f :*: g)  where
  gFromEnv' opts = do
    f' <- gFromEnv' @f opts
    g' <- gFromEnv' @g opts
    return $ liftA2 (:*:) f' g'

instance {-# OVERLAPPING #-} (Selector s, TryParse a) => GFromEnv' (M1 S s (K1 i a)) where
  gFromEnv' opts = do
    let m :: M1 i s f a
        m = undefined
        name = optsFieldLabelModifier opts $ selName m
    envValue <- liftIO $ lookupEnv name
    return $ do
        v <- maybeToEither (UnsetVariable name) envValue
        r <- maybeToEither (FailedToParse name v) (tryParse v)
        return . M1 . K1 $ r

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither _ (Just a) = Right a
maybeToEither b Nothing  = Left b

data FromEnvError
    = UnsetVariable String
    -- ^ A field was unset in the environment
    | FailedToParse String String
    -- ^ Failed to parse a given field from an environment variable
    | AggregateError [FromEnvError]
    -- ^ There was more than one error.
    deriving Eq

instance Show FromEnvError where
    show (UnsetVariable fieldName) =
        "The field " <> fieldName <> " was unset in the environment"
    show (FailedToParse fieldName envValue) =
        "Failed to parse the field " <> fieldName <> " from the value " <> envValue
    show (AggregateError errors) = intercalate ", " (map show errors)
