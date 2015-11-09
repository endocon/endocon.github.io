{-# LANGUAGE OverloadedStrings #-}

import Data.Default (def)
import Hakyll
    ( Compiler, Configuration(..), Context, Identifier, Item
    , Routes, applyAsTemplate, compile, composeRoutes, compressCssCompiler
    , copyFileCompiler , dateField, defaultContext, field, getMetadataField
    , getResourceBody , gsubRoute, hakyllWith , idRoute, itemIdentifier
    , listField, loadAll , loadAndApplyTemplate , match, metadataRoute
    , pandocCompiler , recentFirst, relativizeUrls, route , setExtension
    , templateCompiler
    )


-- | Change some of the default configuration variables.  This makes our
-- project working directory a little cleaner.
hakyllConfig :: Configuration
hakyllConfig = def { providerDirectory = "preprocessed-site"
                   , storeDirectory = ".hakyll-cache"
                   , tmpDirectory = ".hakyll-cache/tmp"
                   , destinationDirectory = "generated-site"
                   }

main :: IO ()
main = hakyllWith hakyllConfig $ do

    -- templates for other routes
    match "templates/*" $ compile templateCompiler

    -- github pages CNAME file
    match "CNAME" $ do
        route idRoute
        compile copyFileCompiler

    -- images
    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    -- CSS
    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    -- Javascript
    match "js/*" $ do
        route idRoute
        compile copyFileCompiler

    -- web fonts
    match "fonts/*" $ do
        route idRoute
        compile copyFileCompiler

    -- index.html
    match "index.html" $ do
        route idRoute
        compile $ do
            indexBody <- getResourceBody
            indexWithContext <- applyAsTemplate defaultContext indexBody
            applyDefaultTemplate defaultContext indexWithContext

-- | Apply the default template and then relativize all the URLs in the
-- resulting html file.
applyDefaultTemplate :: Context a -> Item a -> Compiler (Item String)
applyDefaultTemplate context preTemplateItem = do
    postTemplateItem <- loadAndApplyTemplate defaultTemplate context preTemplateItem
    relativizeUrls postTemplateItem

-- | This is our default site template that all html files should go
-- through.  This default template defines our site HTML \<head\> tag.
defaultTemplate :: Identifier
defaultTemplate = "templates/default.html"
