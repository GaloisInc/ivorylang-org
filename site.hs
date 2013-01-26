--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Data.Monoid         (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler
    
    match "bootstrap/css/*" $ do
        route   $ gsubRoute "bootstrap/"  (const "")
        compile compressCssCompiler
    
    match "bootstrap/js/*" $ do
        route   $ gsubRoute "bootstrap/"  (const "")
        compile copyFileCompiler
    
    match "bootstrap/img/*" $ do
        route   $ gsubRoute "bootstrap/"  (const "")
        compile copyFileCompiler

    match "pages/index.html" $ do
        route   $ gsubRoute "pages/"  (const "")
        compile copyFileCompiler

    match "pages/*.md" $ do
        route   $ gsubRoute "pages/"  (const "") `composeRoutes` 
                  setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

