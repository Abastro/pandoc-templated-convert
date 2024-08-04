module Templating (
  TemplateIO (..),
  runTemplateIO,
  readTemplate,
) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.FilePath
import Text.DocTemplates
import Text.Pandoc
import Text.Pandoc.Writers.Shared

newtype TemplateIO a = TemplateIO (ReaderT (Context T.Text) PandocIO a)
  deriving (Functor, Applicative, Monad, MonadError PandocError, MonadIO, PandocMonad)

instance TemplateMonad TemplateIO where
  getPartial :: FilePath -> TemplateIO T.Text
  getPartial = \case
    path | takeExtension path == ".meta" -> do
      let name = takeBaseName path
      realPath <- TemplateIO $ asks (getField $ T.pack name)
      liftIO $ fromMaybe mempty <$> traverse (T.readFile . T.unpack) realPath
    path -> liftIO $ T.readFile path

runTemplateIO :: Context T.Text -> TemplateIO a -> PandocIO a
runTemplateIO ctxt (TemplateIO act) = runReaderT act ctxt

-- | Reads a template based on given context.
-- The context is only used to look up for the "meta" partial templates.
readTemplate :: Context T.Text -> FilePath -> PandocIO (Template T.Text)
readTemplate ctxt templatePath = do
  templateFile <- liftIO $ T.readFile templatePath
  runTemplateIO ctxt (compileTemplate templatePath templateFile) >>= \case
    Left err -> throwError (PandocTemplateError $ T.pack err)
    Right template -> pure template

