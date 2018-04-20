{-# LANGUAGE OverloadedStrings #-}

module HtmlNotebook.Config
   ( Config(..)
   , readConfig
   , sourceDirs
   , sourceFiles
   , copyDirs
   , copyFiles
   ) where

-- aeson
import Data.Aeson.Types(Value(..),ToJSON(..),FromJSON(..))

-- bytestring
import qualified Data.ByteString as BS(ByteString(..), readFile)

-- yaml
import qualified Data.Yaml as Y(decode)

-- vector
import qualified Data.Vector as V(Vector(..), empty, map, toList)

-- unordered-containers
import qualified Data.HashMap.Strict as HMap(lookup)

-- base
import Data.String(fromString)

-- text
import qualified Data.Text as T(unpack)

newtype Config = Config Value

readConfig :: FilePath -> IO Config
readConfig filepath = do
   text <- BS.readFile filepath
   let t = Y.decode text
   case t of
      Nothing -> error $ "failed to parse " ++ filepath
      Just v -> return $ Config v

readField :: String -> Config -> [String]
readField ss (Config (Object x)) = V.toList $ V.map f ys
   where ys = case HMap.lookup (fromString ss) x of
                 Just (Array ys') -> ys'
                 Nothing -> V.empty
         f :: Value -> FilePath
         f (String text) = T.unpack text

sourceDirs :: Config -> [FilePath]
sourceDirs = readField "sourceDirs"

sourceFiles :: Config -> [FilePath]
sourceFiles = readField "sourceFiles"

copyDirs :: Config -> [FilePath]
copyDirs = readField "copyDirs"

copyFiles :: Config -> [FilePath]
copyFiles = readField "copyFiles"
