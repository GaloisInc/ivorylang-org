--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Data.Monoid         (mappend, mconcat)
import           Hakyll

import           Sidebar
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
    
    match "bootstrap/js/*.js" $ do
        route   $ gsubRoute "bootstrap/"  (const "")
        compile copyFileCompiler
    
    match "jquery/*.js" $ do
        route   $ gsubRoute "jquery/"  (const "js/")
        compile copyFileCompiler
    
    match "bootstrap/img/*" $ do
        route   $ gsubRoute "bootstrap/"  (const "")
        compile copyFileCompiler
    
    match "pages/index.md" $ do
        route   $ gsubRoute "pages/"  (const "") `composeRoutes` 
                  setExtension "html"
        compile $ pandocCompiler >>= (templated "templates/frontpage.html")
    
    match "pages/*.md" $ do
        route   $ gsubRoute "pages/"  (const "") `composeRoutes` 
                  setExtension "html"
        compile $ pandocCompiler >>= (templated "templates/standard.html")

    match "pages/hardware/*.md" $ do
        route   $ gsubRoute "pages/"  (const "") `composeRoutes` 
                  setExtension "html"
        compile $ pandocCompiler >>= (templated "templates/standard.html")

    match "templates/*" $ compile templateCompiler


sitemap :: PageTree
sitemap = Tree "/" "Home" [ (Page "index.html" "Overview" "Overview")
                          , (Tree "hardware" "Hardware" hardwarepages)
                          ]
  where
  hardwarepages = [ Page "index.html" "Overview" "Hardware Overview"
                  , Page "px4fmu.html" "PX4FMU" "PX4FMU Flight Controller"
                  , Page "ardrone.html" "AR Drone" "AR Drone Airframe"
                  , Page "arducopter.html" "ArduCopter" "ArduCopter Airframe"
                  ]

navbar :: String
navbar = "<ul class=\"nav\"> " ++
         "<li><a href=\"/index.html\">Home</a></li> " ++
         "<li><a href=\"/about.html\">About</a></li> " ++
         "</ul>"

templated :: Identifier -> Item String -> Compiler (Item String)
templated t input = loadAndApplyTemplate t ctx input >>= relativizeUrls
  where
  ctx :: Context String
  ctx = mconcat
    [ field "navbar"    $ \_    -> return navbar
    , field "sidebar"   $ \item -> return (sidebarHTML sitemap item)
    , field "directory" $ \item -> return (itemDirectory item)
    , field "filepath"  $ \item -> return (itemFilePath item)
    , constField "copyright" "<p>&copy; Galois Inc. 2013</p>"
    , defaultContext
    ]

