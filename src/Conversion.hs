{-# LANGUAGE LambdaCase #-}

module Conversion (
  readPandoc,
  writePandoc,
  pandocToMeta,
  readMeta,
  writeMeta,
  metaAsContext,
  pandocToContext,
  convertTikzFilter,
  basedirFilter,
) where

import Control.Monad.Except
import Data.Aeson
import Data.Digest.Pure.SHA
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import System.Directory
import System.FilePath
import System.IO.Temp qualified as Temp
import System.Process
import Templating
import Text.DocLayout
import Text.DocTemplates
import Text.Pandoc
import Text.Pandoc.Walk (walkPandocM)
import Text.Pandoc.Writers.Shared

-- | Reads pandoc based on extensions, supports markdown and LaTeX.
readPandoc :: ReaderOptions -> FilePath -> PandocIO Pandoc
readPandoc readerOpts path = do
  file <- liftIO $ T.readFile path
  case takeExtension path of
    ".md" -> readMarkdown readerOpts file
    ".tex" -> readLaTeX readerOpts file
    _ -> readHtml readerOpts file

-- | Writes pandoc. Currently only supports HTML5.
writePandoc :: WriterOptions -> FilePath -> Pandoc -> PandocIO ()
writePandoc writerOpts path pandoc = do
  file <- writeHtml5String writerOpts pandoc
  liftIO $ T.writeFile path file

pandocToMeta :: Pandoc -> Meta
pandocToMeta (Pandoc meta _) = meta

readMeta :: FilePath -> PandocIO Meta
readMeta path = liftIO (decodeFileStrict path) >>= \case
  Just meta -> pure meta
  Nothing -> throwError (PandocParseError "failed parsing metadata")

writeMeta :: FilePath -> Meta -> PandocIO ()
writeMeta path meta = liftIO $ encodeFile path meta

-- | Converts meta into contexts that can be looked into.
metaAsContext :: WriterOptions -> Meta -> PandocIO (Context T.Text)
metaAsContext opts meta = do
  metaToContext' blocksToText (blocksToText . pure . Plain) meta
 where
  blocksToText blocks = literal <$> writeHtml5String opts (Pandoc nullMeta blocks)

pandocToContext :: WriterOptions -> Pandoc -> PandocIO (Context T.Text)
pandocToContext opts (Pandoc meta _) = do
  metaToContext' blocksToText (blocksToText . pure . Plain) meta
 where
  blocksToText blocks = literal <$> writeHtml5String opts (Pandoc nullMeta blocks)

-- | Tikz conversion filter.
-- Requires output file to be set to output images on correct path,
-- and need to relativize paths.
convertTikzFilter :: Pandoc -> PandocIO Pandoc
convertTikzFilter = walkPandocM processTikz

-- | Find tikz lines and converts them into svg.
-- The svg is saved at the directory of output file.
processTikz :: Block -> PandocIO Block
processTikz = \case
  block@(RawBlock _ contents) | any (`T.isPrefixOf` contents) ["\\begin{tikzpicture}", "\\begin{tikzcd}"] -> do
    directory <- maybe "." takeDirectory <$> getOutputFile
    template <- readTemplate mempty "templates/tikz.tex"
    imgSource <- writeLaTeX def{writerTemplate = Just template} (Pandoc nullMeta [block])
    let imagePath = directory </> imageName <.> "svg"
    intoSvg imagePath imgSource
    pure $ Para [Image nullAttr [] (T.pack $ imageName <.> "svg", T.pack imageName)]
   where
    imageName = showDigest . sha1 $ LT.encodeUtf8 . LT.fromStrict $ contents
    intoSvg imgPath source = liftIO $ do
      prevDirectory <- getCurrentDirectory
      Temp.withSystemTempDirectory imageName $ \dir -> do
        withCurrentDirectory dir $ do
          T.writeFile "tikz.tex" source
          callProcess "pdflatex" ["tikz.tex"]
          callProcess "pdf2svg" ["tikz.pdf", prevDirectory </> imgPath]
          putStrLn $ "written into " <> imgPath
  x -> pure x

-- | Filter that corrects the base directory when the site is not at root.
basedirFilter :: FilePath -> Pandoc -> PandocIO Pandoc
basedirFilter basedir input = do
  walkPandocM (pure . replaceBasedir basedir) input

-- | Replaces the base directory.
replaceBasedir :: FilePath -> Inline -> Inline
replaceBasedir basedir = \case
  Link attrs txt (url, name) -> Link attrs txt (replaceAsText url, name)
  Image attrs txt (url, name) -> Image attrs txt (replaceAsText url, name)
  inline -> inline
 where
  replaceAsText = T.pack . replaces . T.unpack
  replaces url
    | isAbsolute url = basedir </> dropDrive url
    | otherwise = url
