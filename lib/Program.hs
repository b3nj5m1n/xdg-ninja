{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Program where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy     as B
import qualified Data.Text                as T
import           GHC.Generics

data File = File
  { path         :: String,
    supportLevel :: SupportLevel,
    help         :: String
  }
  deriving (Generic, Show)

instance ToJSON File where
  toEncoding (File path supportLevel help) = pairs ("path" .= path <> "movable" .= supportLevel <> "help" .= help)

data Program = Program
  { name  :: T.Text,
    files :: [File]
  }
  deriving (Generic, Show)

instance ToJSON Program where
  toEncoding = genericToEncoding defaultOptions

save :: Program -> IO ()
save program = do
  let path = ("./programs/" ++ (T.unpack (name program)) ++ ".json")
  B.writeFile path (encodePretty program)

data SupportLevel = Unsupported | Alias | EnvVars | Supported
  deriving (Generic, Show)

instance ToJSON SupportLevel where
  toEncoding Unsupported = toEncoding ( Bool False )
  toEncoding _           = toEncoding ( Bool True )

