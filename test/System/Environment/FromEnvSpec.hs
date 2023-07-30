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

clearEnvs :: IO ()
clearEnvs = getEnvironment >>= mapM_ unsetEnv . fmap fst

unwrapEither :: Either a b -> b
unwrapEither (Left _)  = error "tried to unwrap left"
unwrapEither (Right b) = b
