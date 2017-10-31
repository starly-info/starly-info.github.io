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
import        Config                (feedConfig)
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

    create ["sitemap.xml"] sitemapBehavior

    forM_ Multilang.langs $ \lang -> do
      let slang = show lang

      match ("about*" .||. "tos*" .||. "privacy*") $ globalBehavior lang
      match "static/example.html"                  $ globalBehavior lang

      match "subscribe*" $ subscribeBehavior lang

      match (fromGlob $ "index-"  ++ slang ++ ".html" ) $ indexBehavior   lang
      match (fromGlob $ "course/" ++ slang ++ "/*"    ) $ courseBehavior  lang

      create [fromFilePath ("gen/" ++ slang ++ "/archive.html")] (archiveBehavior          lang)
      create [fromFilePath ("gen/" ++ slang ++ "/rss.xml")]      (feedBehavior renderRss   lang)
      create [fromFilePath ("gen/" ++ slang ++ "/atom.xml")]     (feedBehavior renderAtom  lang)

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
                    $ zip (keys Multilang.dbTranslations) $ mapMaybe (Data.Map.lookup l) (elems Multilang.dbTranslations)

postCtx :: Context String
postCtx =
    titleNoDateField "titleNoDate"                    `mappend`
    constField "host" "https://starly-info.github.io" `mappend`
    dateField "created" "%d %b %Y"                    `mappend`
    modificationTimeField "modified" "%d %b %Y"       `mappend`
    defaultContext

postCtxWithLanguage :: Language -> Context String
postCtxWithLanguage l = mconcat $ [
                                    titleNoDateField "titleNoDateField",
                                    constField "host" "https://starly-info.github.io",
                                    dateField "created" "%d %b %Y",
                                    modificationTimeField "modified" "%d %b %Y",
                                    defaultCtxWithLanguage l
                                  ]

defaultCtxWithLanguage :: Language -> Context String
defaultCtxWithLanguage l = mconcat $ languageContext l ++ [defaultContext]

titleNoDateField :: String -> Context a
titleNoDateField = mapContext removeDate . pathField

indexCtx l posts = mconcat $ [
                                titleNoDateField "titleNoDate",
                                constField "lang" l,
                                listField "posts" postCtx (return posts),
                                defaultCtxWithLanguage (Multilang.fromStringToLanguage l)
                             ]

removeDate :: String -> String
removeDate i = removeDate' 0 (takeBaseName i)
  where removeDate' :: Int -> String -> String
        removeDate' 3 s = s
        removeDate' _ [] = ""
        removeDate' acc (x:xs)
          | x == '-'  = removeDate' (acc+1) xs
          | otherwise = removeDate' acc xs

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
      posts <- recentFirst =<< loadAll (fromGlob $ "course/" ++ (show l) ++ "/*")
      let ctx = indexCtx (show l) posts

      getResourceBody
          >>= applyAsTemplate ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= applyFilter abbreviationFilter
          >>= relativizeUrls

courseBehavior :: Language -> Rules ()
courseBehavior l = do
  route   $ setExtension "html"
  compile $ pandocCompilerWith withLinkAtt defaultHakyllWriterOptions
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate (fromFilePath $ "templates/" ++ (show l) ++ "/post.html") (postCtxWithLanguage l)
      >>= loadAndApplyTemplate "templates/payments.html"                                  defaultContext
      >>= loadAndApplyTemplate "templates/facebook.html"                                  defaultContext
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

subscribeBehavior :: Language -> Rules ()
subscribeBehavior l = do
  route   $ setExtension "html"
  compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/mailchimp.html"  defaultContext
      >>= loadAndApplyTemplate "templates/default.html"   (defaultCtxWithLanguage l)
      >>= applyFilter abbreviationFilter
      >>= relativizeUrls

archiveBehavior :: Language -> Rules ()
archiveBehavior language = do
    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAll (fromGlob $ "course/" ++ (show language) ++ "/*")
        let ctx = indexCtx (show language) posts

        makeItem ""
            >>= loadAndApplyTemplate langTemplate ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= applyFilter abbreviationFilter
            >>= relativizeUrls
      where
        langTemplate = fromFilePath $ "templates/" ++ (show language) ++ "/archive.html"

sitemapBehavior :: Rules ()
sitemapBehavior = do
    route   idRoute
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots (fromGlob $ "course/*/*") "content"
      let ctx = mconcat $ [
                    listField "posts" postCtx (return posts)
                 ,  constField "host" "https://starly-info.github.io"
                 ,  defaultContext
                 ]

      makeItem ""
          >>= loadAndApplyTemplate "templates/sitemap.xml" ctx
          >>= applyFilter abbreviationFilter
          >>= relativizeUrls

feedBehavior :: (FeedConfiguration
                  -> Context String
                  -> [Item String]
                  -> Compiler (Item String)) -> Language -> Rules ()
feedBehavior render language = do
    route idRoute
    compile $
        loadAllSnapshots (fromGlob $ "course/" ++ (show language) ++ "/*") "content"
        >>= fmap (take 10) . recentFirst
        >>= mapM (applyFilter (protectCDATA . abbreviationFilter))
        >>= render (feedConfig language) feedCtx
      where

        feedCtx :: Context String
        feedCtx = postCtx `mappend` bodyField "description"

        protectCDATA :: String -> String
        protectCDATA = replaceAll "]]>" (const "]]&gt;")


-- }}}
