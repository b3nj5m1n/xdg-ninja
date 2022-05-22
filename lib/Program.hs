{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Program where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy     as B
import qualified Data.Text                as T
import           GHC.Generics

instance FromJSON File where
  parseJSON (Object v) = File
    <$> v .: "path"
    <*> v .: "movable"
    <*> v .: "help"

data Program = Program
  { name  :: T.Text,
    files :: [File]
  }
  deriving (Generic, Show)

instance ToJSON Program where
  toJSON (Program name files) = object [ "name" .= name, "files" .= files ]
  toEncoding (Program name files) = pairs ("name" .= name <> "files" .= files)
instance FromJSON Program

data File = File
  { path         :: String,
    supportLevel :: SupportLevel,
    help         :: String
  }
  deriving (Generic, Show)

instance ToJSON File where
  toJSON (File path supportLevel help) = object [ "path" .= path, "movable" .= supportLevel, "help" .= help ]
  toEncoding (File path supportLevel help) = pairs ("path" .= path <> "movable" .= supportLevel <> "help" .= help)

data SupportLevel = Unsupported | Alias | EnvVars | Supported
  deriving (Generic, Show)

instance ToJSON SupportLevel where
  toJSON Unsupported = toJSON (Bool False)
  toJSON _           = toJSON (Bool True)
  toEncoding Unsupported = toEncoding ( Bool False )
  toEncoding _           = toEncoding ( Bool True )
instance FromJSON SupportLevel where
  parseJSON (Bool False) = return Unsupported
  parseJSON (Bool True)  = return EnvVars

makeFilename :: T.Text -> T.Text
makeFilename s = T.pack ( "./programs/" ++ T.unpack s ++ ".json" )

save :: T.Text -> Program -> IO ()
save filename program = do
  B.writeFile (T.unpack filename) (encodePretty program)

readProgram :: String -> IO ( Maybe Program )
readProgram filename = do
    json_data <- B.readFile filename
    return (decode json_data)
