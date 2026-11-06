{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
module System.Environment.FromEnvSpec (main, spec) where

import           Data.Either                (isLeft, isRight)
import           GHC.Generics
import           System.Environment         (getEnvironment, setEnv, unsetEnv)
import           Test.Hspec                 (Spec, after_, describe, hspec, it,
                                             shouldBe, shouldSatisfy)

import           System.Environment.FromEnv

main :: IO ()
main = hspec spec

spec :: Spec
spec = fromEnvSpec >> gFromEnvSpec

fromEnvSpec :: Spec
fromEnvSpec =
  describe "fromEnv" $ after_ clearEnvs $ do
    it "returns Nothing when the necessary environment variables are not set" $ do
      config <- fromEnv @Config
      config `shouldSatisfy` isLeft

    it "returns Nothing when only one environment variable is missing" $ do
      setEnv "CONFIG_DB_URL" "hello"
      config <- fromEnv @Config
      config `shouldBe` Left (UnsetVariable "CONFIG_API_KEY")

    it "returns the configuration object when all necessary variables are set" $ do
      setEnv "CONFIG_DB_URL" "hello"
      setEnv "CONFIG_API_KEY" "world"
      config <- fromEnv
      config `shouldSatisfy` isRight
      unwrapEither config `shouldBe` Config "hello" "world"

    describe "the tuple instances" $ do
      it "return a list of errors when more than one value failed to build" $ do
        Left (AggregateError [e1, e2]) <- fromEnv @(Config, StorageConfig)
        e1 `shouldBe` UnsetVariable "CONFIG_DB_URL"
        e2 `shouldBe` UnsetVariable "S3_BUCKET_URL"

      it "return the correct values when setting valid environment variables" $ do
          setEnv "CONFIG_DB_URL" "hello"
          setEnv "CONFIG_API_KEY" "world"
          setEnv "S3_BUCKET_URL" "https://google.com"

          (config, storageConfig) <- unwrapEither <$> fromEnv

          config `shouldBe` Config "hello" "world"
          storageConfig `shouldBe` StorageConfig "https://google.com"


gFromEnvSpec :: Spec
gFromEnvSpec =
    describe "gFromEnv" $ after_ clearEnvs $ do
        it "returns the configuration object when using a custom field label modifier" $ do
            setEnv "configDbURL" "hello"
            setEnv "configApiKey" "world"
            config <- gFromEnv (defaultEnvOpts { optsFieldLabelModifier = id })
            config `shouldSatisfy` isRight
            unwrapEither config `shouldBe` Config "hello" "world"

data Config = Config
  { configDbURL  :: !String
  , configApiKey :: !String
  }
  deriving (Eq, Show, Generic, FromEnv)

newtype StorageConfig = StorageConfig
  { s3BucketUrl :: String
  }
  deriving (Eq, Show, Generic, FromEnv)

clearEnvs :: IO ()
clearEnvs = getEnvironment >>= mapM_ unsetEnv . fmap fst

unwrapEither :: Either a b -> b
unwrapEither (Left _)  = error "tried to unwrap left"
unwrapEither (Right b) = b
