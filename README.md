# from-env

Haskell package to construct datatypes from environment variables.

In many applications you'll have an `.env` file or set environment variables
some way. These environment variables usually contain configuration data such
as database connection urls, secrets; etc.

Next, you make a configuration data type to hold this variables so your
application can access them. Normally you'd have a bunch of calls to `lookupEnv`
in order to build your data type. This is tedious and error-prone. Thankfully,
in Haskell we can do better!

```haskell
import GHC.Generics
import System.Environment.FromEnv

data Config = Config
    { configDbUrl     :: !String
    , configApiSecret :: !String
    }
    deriving Generic

instance FromEnv Config

main = do
    config <- fromEnv
    -- do something with config
```

And that's it! By deriving `Generic` you can now create an instance of `FromEnv`
for free. Check out the haddocks for more.

## License

MIT
