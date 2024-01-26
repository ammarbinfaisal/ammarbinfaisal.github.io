--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Identity (runIdentity)
import Data.Monoid (mappend)
import Data.Text (Text)
import qualified Data.Text as T
import Hakyll
  ( Compiler,
    Context,
    Item (itemBody, itemIdentifier),
    Tags,
    applyAsTemplate,
    buildTags,
    compile,
    compressCssCompiler,
    constField,
    copyFileCompiler,
    create,
    dateField,
    defaultContext,
    defaultHakyllReaderOptions,
    defaultHakyllWriterOptions,
    fromCapture,
    fromList,
    getMetadataField,
    getResourceBody,
    getUnderlying,
    hakyll,
    idRoute,
    listField,
    loadAll,
    loadAndApplyTemplate,
    loadSnapshot,
    makeItem,
    match,
    pandocCompiler,
    pandocCompilerWith,
    recentFirst,
    relativizeUrls,
    route,
    setExtension,
    tagsField,
    templateBodyCompiler, tagsRules,
  )
import Text.Pandoc.Highlighting (Style, styleToCss, tango)
import Text.Pandoc.Options (ReaderOptions (..), WriterOptions (..))
import Text.Pandoc.Templates
  ( Template,
    compileTemplate,
  )
import Unicode.Char.General (isMark)
import Unicode.Char.Normalization

style = tango

pandocCompiler' :: Compiler (Item String)
pandocCompiler' = do
  underlying <- getUnderlying
  toc <- getMetadataField underlying "toc"
  pandocCompilerWith
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
      { writerHighlightStyle = Just style,
        writerTableOfContents = True,
        writerTOCDepth = 2,
        writerTemplate = Just tocTemplate,
        writerNumberSections = True,
        writerSectionDivs = True
      }

-- if not arabic, remove accents
-- removeAccents = filter (not . isMark) . concatMap (decompose Canonical)
removeAccents =
  concatMap
    ( filter
        ( or . sequence [not . isMark, isArabic]
        )
        . decompose Canonical
    )
  where
    isArabic c = c >= '\x0600' && c <= '\x06FF'

tocTemplate :: Template Text
tocTemplate =
  either error id . runIdentity . compileTemplate "" $
    T.unlines
      [ "<div class=\"toc\">",
        "$toc$",
        "</div>",
        "$body$"
      ]

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "fonts/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match (fromList ["about.markdown", "contact.markdown"]) $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  tags <- buildTags "posts/*" (fromCapture "tags/*.html")

  tagsRules tags $ \tag pattern -> do
    let title = "Posts tagged \"" ++ tag ++ "\""
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let ctx =
            constField "title" title
              `mappend` listField "posts" (postCtxWithTags tags) (return posts)
              `mappend` defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  let postCtx = postCtxWithTags tags

  match "posts/*" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler'
        >>= return . fmap removeAccents
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  create ["css/syntax.css"] $ do
    route idRoute
    compile $ do
      makeItem $ styleToCss style

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Archives"
              `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts)
              `mappend` defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
