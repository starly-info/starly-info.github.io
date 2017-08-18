{-# LANGUAGE OverloadedStrings #-}
import        Hakyll

import        Control.Monad         (forM_)
import        Data.Monoid           ((<>))
import        Data.List             (isInfixOf)
import        Data.Map              (keys, elems, lookup)
import        Data.Maybe            (mapMaybe)
import        Text.Pandoc
import        System.FilePath.Posix (takeBaseName, takeDirectory, (</>),
                                      splitFileName)

import        Abbreviations         (abbreviationFilter)
import        Config
import        Multilang

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    match "static/img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "static/css/*" $ do
        route   idRoute
        compile $ compressCssCompiler >>= relativizeUrls

    -- Default index page (a version index-LANGUAGE must exist)
    match "index.html" $ indexBehavior English


    forM_ langs $ \lang -> do
      let slang = show lang

      match ("about*" .||. "tos*" .||. "privacy*")         $ globalBehavior lang

      match (fromGlob $ "index-" ++ slang ++ ".html"   )  $ indexBehavior      lang
      match (fromGlob $ "newsletter/" ++ slang ++ "/*" )  $ newsletterBehavior lang

      create [fromFilePath ("gen/" ++ slang ++ "/archive.html")]  (archiveBehavior      lang)
      create [fromFilePath ("gen/" ++ slang ++ "/rss.xml")]           (feedBehavior renderRss   lang)
      create [fromFilePath ("gen/" ++ slang ++ "/atom.xml")]          (feedBehavior renderAtom  lang)

    match "templates/*"         $ compile templateCompiler
    match "templates/*/*.html"  $ compile templateCompiler


-----------------------------------------------------------------------------{{{
-- Utils

--- apply a filter before render
applyFilter :: (Monad m, Functor f) => (String -> String) -> f String -> m (f String)
applyFilter transformator s = return $ fmap transformator s

-- }}}


-----------------------------------------------------------------------------{{{
-- Ctx

-- replace mappend with mconcat see src/Hakyll/Web/Feed.hs
languageContext l = map (\ (k, v) -> constField k v)
                    $ zip (keys dbTranslations) $ mapMaybe (Data.Map.lookup l) (elems dbTranslations)

postCtx :: Context String
postCtx =
    dateField "created" "%d %b %Y" `mappend`
    modificationTimeField "modified" "%d %b %Y" `mappend`
    defaultContext

postCtxWithLanguage :: Language -> Context String
postCtxWithLanguage l = mconcat $ [
                                    dateField "created" "%d %b %Y",
                                    modificationTimeField "modified" "%d %b %Y",
                                    defaultCtxWithLanguage l
                                  ]

defaultCtxWithLanguage :: Language -> Context String
defaultCtxWithLanguage l = mconcat $ languageContext l ++ [defaultContext]

indexCtx l posts = mconcat $ [
                                listField "posts" postCtx (return posts),
                                defaultCtxWithLanguage l
                             ]

-- }}}


-----------------------------------------------------------------------------{{{
-- Simplify URL

--- replace url of the form foo/bar/index.html by foo/bar
removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap (withUrls removeIndexStr) item

removeIndexStr :: String -> String
removeIndexStr url = case splitFileName url of
  (dir, "index.html") | isLocal dir -> dir
  _                                 -> url
  where isLocal uri = not ("://" `isInfixOf` uri)

-- }}}


-----------------------------------------------------------------------------{{{
-- Behavior

indexBehavior :: Language -> Rules ()
indexBehavior l = do
  route idRoute
  compile $ do
      posts <- recentFirst =<< loadAll (fromGlob $ "newsletter/" ++ (show l) ++ "/*")
      let ctx = indexCtx l posts

      getResourceBody
          >>= applyAsTemplate ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= applyFilter abbreviationFilter
          >>= relativizeUrls

newsletterBehavior :: Language -> Rules ()
newsletterBehavior l = do
  route   $ setExtension "html"
  compile $ pandocCompilerWith withLinkAtt defaultHakyllWriterOptions
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate (fromFilePath $ "templates/" ++ (show l) ++ "/post.html") (postCtxWithLanguage l)
      >>= loadAndApplyTemplate "templates/mailchimp.html"                                 defaultContext
      >>= loadAndApplyTemplate "templates/default.html"                                  (postCtxWithLanguage l)
      >>= applyFilter abbreviationFilter
      >>= relativizeUrls
      >>= removeIndexHtml
  where
    withLinkAtt = defaultHakyllReaderOptions
      { readerDefaultImageExtension = "+link_attributes"
      }

globalBehavior :: Language -> Rules ()
globalBehavior l = do
  route   $ setExtension "html"
  compile $ pandocCompiler
      >>= loadAndApplyTemplate (fromFilePath $ "templates/" ++ (show l) ++ "/donation.html") (defaultCtxWithLanguage l)
      >>= loadAndApplyTemplate "templates/default.html" (defaultCtxWithLanguage l)
      >>= applyFilter abbreviationFilter
      >>= relativizeUrls

archiveBehavior :: Language -> Rules ()
archiveBehavior language = do
    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAll (fromGlob $ "newsletter/" ++ (show language) ++ "/*")
        let ctx = indexCtx language posts

        makeItem ""
            >>= loadAndApplyTemplate langTemplate ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= applyFilter abbreviationFilter
            >>= relativizeUrls
      where
        langTemplate = fromFilePath $ "templates/" ++ (show language) ++ "/archive.html"


feedBehavior :: (FeedConfiguration
                  -> Context String
                  -> [Item String]
                  -> Compiler (Item String)) -> Language -> Rules ()
feedBehavior render language = do
    route idRoute
    compile $
        loadAllSnapshots (fromGlob $ "newsletter/" ++ (show language) ++ "/*") "content"
        >>= fmap (take 10) . recentFirst
        >>= mapM (applyFilter (protectCDATA . abbreviationFilter))
        >>= render (feedConfig language) feedCtx
      where

        feedCtx :: Context String
        feedCtx = postCtx `mappend` bodyField "description"

        protectCDATA :: String -> String
        protectCDATA = replaceAll "]]>" (const "]]&gt;")


-- }}}
