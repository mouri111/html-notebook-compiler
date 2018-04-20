{-# LANGUAGE OverloadedStrings #-}

module HtmlNotebook.Compiler
   ( compiler
   ) where

-- directory
import System.Directory(listDirectory, getCurrentDirectory,createDirectoryIfMissing,copyFile)

-- base
import Data.Foldable(forM_,foldrM)
import Data.Traversable(for)
import Data.Monoid(mappend,(<>))
import System.IO.Unsafe(unsafePerformIO)
import Control.Monad(forM)
import GHC.Generics(Generic)
import Data.Maybe(mapMaybe)
import Data.List(sortBy)

-- text
import qualified Data.Text as T(pack,unpack,Text(..),replace,replicate,filter,length)
import qualified Data.Text.IO as T(readFile,putStrLn,writeFile)

-- pandoc
import Text.Pandoc.Readers.Markdown(readMarkdown)
import Text.Pandoc.Readers.HTML(readHtml)
import Text.Pandoc.Writers.HTML(writeHtml5String)
import Text.Pandoc.Class(runPure,runIO,PandocIO(..),PandocPure(..),PandocMonad(..))
import Text.Pandoc.Extensions(pandocExtensions,enableExtension,Extension(..))
import Text.Pandoc.Options(ReaderOptions(..),WriterOptions(..),HTMLMathMethod(..))
import Text.Pandoc.Templates(renderTemplate')
import Text.Pandoc.Writers.Shared(metaToJSON')
import Text.Pandoc.Error(PandocError(..),handleError)

-- pandoc-types
import qualified Text.Pandoc.Builder as P(fromList,setMeta)
import Text.Pandoc.Definition(Pandoc(..),MetaValue(..),Inline(..),Block(..),Inline(..),Meta(..),nullMeta)

-- data-default
import Data.Default(def)

-- transformers
import Control.Monad.IO.Class(MonadIO(..))

-- aeson
import Data.Aeson.Types(Value(..),ToJSON(..),FromJSON(..))

-- filepath
import System.FilePath.Posix(replaceExtension,takeFileName,takeDirectory,takeExtension,takeBaseName)

-- bytestring
import qualified Data.ByteString as BS(ByteString(..),readFile)

-- yaml
import qualified Data.Yaml as Y(decode)

-- process
import System.Process(system)

-- doctemplates
import Text.DocTemplates(TemplateTarget(..),applyTemplate)

-- mtl
import Control.Monad.Except(MonadError(..))

-- containers
import qualified Data.Map as Map(fromList, singleton)

-- html-notebook-compiler
import HtmlNotebook.Config(Config(..), readConfig, sourceDirs, sourceFiles, copyDirs, copyFiles)

readerOptions :: ReaderOptions
readerOptions = def{readerExtensions=pandocExtensions}

writerOptions :: WriterOptions
writerOptions = def{writerExtensions=pandocExtensions,writerHTMLMathMethod=KaTeX ""}

blocksToValue :: Monad m => [Block] -> m Value
blocksToValue _ = return $ toJSON ()

inlinesToValue :: Monad m => [Inline] -> m Value
inlinesToValue xs = do
   let f x = case x of
                Str s -> toJSON s
                _ -> toJSON ()
   let ys = map f xs
   return $ toJSON ys

defaultConfigFileName :: FilePath
defaultConfigFileName = "html-notebook.yaml"

renderTemplate'' :: (PandocMonad m, ToJSON a, TemplateTarget b) => T.Text -> a -> m b
renderTemplate'' template context =
   case applyTemplate template context of
      Left e  -> throwError (PandocTemplateError e)
      Right r -> return r

data ReadType = ReadHtml | ReadMarkdown
   deriving (Eq, Show)

data Inst = Inst
   { instFileDir :: FilePath
   , instFileName :: FilePath
   , instBaseName :: FilePath
   , instSourceText :: T.Text
   , instReadType :: ReadType
   , instTemplates :: [T.Text]
   } deriving (Eq, Show)

genGlobalMeta :: [Inst] -> Meta
genGlobalMeta xs = Meta $ Map.singleton "insts" $ MetaList $ map instToMeta xs

instToMeta :: Inst -> MetaValue
instToMeta (Inst fileDir fileName fileBase _ readType _) = MetaMap $ Map.fromList
   [ ("dir",MetaString fileDir)
   , ("name",MetaString fileName)
   , ("base",MetaString fileBase)
   , ("readType",MetaString $ show readType)
   ]

buildStage1 :: [Inst] -> Either PandocError [Pandoc]
buildStage1 xs = runPure $
   forM xs $ \x -> do
      let (Inst fileDir fileName fileBase sourceText readType templates) = x
      case readType of
         ReadHtml     -> readHtml def sourceText
         ReadMarkdown -> readMarkdown readerOptions sourceText

genMeta :: Inst -> Pandoc -> MetaValue
genMeta inst pandoc = MetaMap $ unMeta meta <> Map.fromList
   [ ("dir",MetaString fileDir)
   , ("name",MetaString fileName)
   , ("base",MetaString fileBase)
   , ("readType",MetaString $ show readType)
   ]
   where
      (Pandoc meta _) = pandoc
      (Inst fileDir fileName fileBase _ readType _) = inst

buildStage2 :: Config -> [Inst] -> [Pandoc] -> Meta
buildStage2 config xs ys = Meta $ mconcat $ filesMap:zs
   where
      filesMap = Map.singleton "files" $ MetaList $ zipWith genMeta xs ys
      zs = flip map (sourceDirs config) $ \dir -> Map.singleton dir $ MetaList $
         mapMaybe (\(inst',metav') -> if instFileDir inst' == dir && instFileName inst' /= "index.html" then Just metav' else Nothing) metaList
      as = zip xs $ zip xs ys
      bs = sortBy (flip (\(Inst _ _ fileBase1 _ _ _,_) (Inst _ _ fileBase2 _ _ _,_) -> compare fileBase1 fileBase2)) as
      metaList = map (\(a,(x,y))->(a,genMeta x y)) bs

buildStage3 :: [Inst] -> [Pandoc] -> Meta -> Either PandocError [(FilePath,T.Text)]
buildStage3 xs ys globalMeta = runPure $
   forM (zip xs ys) $ \(inst,pandoc) -> do
      let (Inst fileDir fileName fileBase sourceText readType templates) = inst
      let (Pandoc meta blocks) = pandoc
      body <- writeHtml5String writerOptions pandoc
      val' <- metaToJSON' blocksToValue inlinesToValue globalMeta
      body' <- renderTemplate'' body val' :: PandocPure T.Text
      let f :: T.Text -> String -> PandocPure String
          f template doc' = do
             let meta' = P.setMeta "body" (MetaInlines [Str doc']) meta
             value <- metaToJSON' blocksToValue inlinesToValue (meta' <> globalMeta)
             renderTemplate'' template value :: PandocPure String
      res <- foldrM f (T.unpack body') templates
      let filePath = replaceExtension ("_site/" ++ fileDir ++ "/" ++ fileName) ".html"
      let re = relativize $ if fileDir == "." then 0 else length (filter (=='/') fileDir) + 1
      return (filePath, re $ T.pack res)

build :: Config -> [Inst] -> Either PandocError [(FilePath,T.Text)]
build config xs = do
   ps <- buildStage1 xs
   let meta = buildStage2 config xs ps
   buildStage3 xs ps meta

relativize :: Int -> T.Text -> T.Text
relativize d = T.replace "href=\"/" ("href=\"./" <> T.replicate d "../")

mkFileInst :: String -> FilePath -> FilePath -> IO Inst
mkFileInst templateName fileDir fileName = do
   text <- T.readFile $ fileDir ++ "/" ++ fileName
   print fileDir
   let ext = takeExtension fileName
   let readType | ext == ".md" = ReadMarkdown
                | ext == ".html" = ReadHtml
                | otherwise = error $ "not corresponded extension " ++ ext
   template1 <- T.readFile $ "template/" ++ templateName ++ ".html"
   template2 <- T.readFile "template/template.html"
   let templates = [template2,template1]
   let fileBase = takeBaseName fileName
   case readType of
      ReadMarkdown -> return $ Inst fileDir fileName fileBase text readType templates
      ReadHtml -> return $ Inst fileDir fileName fileBase (T.pack "$body$") readType $ templates ++ [text]

writeFile' :: FilePath -> T.Text -> IO ()
writeFile' path text = do
   createDirectoryIfMissing True $ takeDirectory path
   T.writeFile path text

compiler :: IO ()
compiler = do
   config <- readConfig "html-notebook.yaml"
   xs1 <- forM (sourceDirs config) $ \dir -> do
      files <- listDirectory dir
      let files' = map (\x -> (dir,x)) files
      forM files' (\(dir,x) -> mkFileInst dir dir x)
   xs2 <- forM (sourceFiles config) $ \filePath ->
      mkFileInst "default" (takeDirectory filePath) (takeFileName filePath)
   let xs = concat xs1 ++ xs2
   val <- metaToJSON' blocksToValue inlinesToValue $ genGlobalMeta xs
   print val
   let t = build config xs
   ys <- handleError t
   forM_ ys $ uncurry writeFile'
   forM_ (copyDirs config) $ \x -> system $ "cp -r " ++ x ++ " _site"
   forM_ (copyFiles config) $ \x -> copyFile x $ "_site/" ++ x
