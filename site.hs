--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Data.Monoid         (mappend, mconcat)
import           Hakyll

import           Sidebar
--------------------------------------------------------------------------------

standardPandocPagesSubdir d = do
    match (fromGlob ("pages/" ++ d ++ "*.md")) $ do
        route   $ gsubRoute "pages/"  (const "") `composeRoutes`
                  setExtension "html"
        compile $ pandocCompiler >>= (templated "templates/standard.html")

config :: Configuration
config = defaultConfiguration { deployCommand = deploy }
  where deploy = "scp -r _site/* cerf.galois.com:/srv/www/smaccmpilot.org/public_html/"

main :: IO ()
main = hakyllWith config $ do

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "bootstrap/css/*" $ do
        route   $ gsubRoute "bootstrap/" (const "")
        compile compressCssCompiler

    match "bootstrap/js/*.js" $ do
        route   $ gsubRoute "bootstrap/" (const "")
        compile copyFileCompiler

    match "jquery/*.js" $ do
        route   $ gsubRoute "jquery/" (const "js/")
        compile copyFileCompiler

    match "bootstrap/img/*" $ do
        route   $ gsubRoute "bootstrap/" (const "")
        compile copyFileCompiler

    match "pages/index.md" $ do
        route   $ gsubRoute "pages/"  (const "") `composeRoutes`
                  setExtension "html"
        compile $ pandocCompiler >>= (templated "templates/frontpage.html")

    match "templates/*" $ compile templateCompiler

    standardPandocPagesSubdir ""
    standardPandocPagesSubdir "hardware/"
    standardPandocPagesSubdir "software/"
    standardPandocPagesSubdir "languages/"

sitemap :: PageTree
sitemap = Tree "/" "Home" [ (Page "index.html" "Overview")
                          , (Tree "languages"  "Languages" langpages)
                          , (Tree "software"   "Software"  softwarepages)
                          , (Tree "hardware"   "Hardware"  hardwarepages)
                          , (Page "about.html" "About")
                          ]
  where
  hardwarepages =
    [ Page "index.html"            "Overview"
    , Page "shoppinglist.html"     "Shopping List"
    , Page "flightcontroller.html" "Flight Controller"
    , Page "blackmagic.html"       "Debugger"
    ]
  softwarepages =
    [ Page "index.html"           "Introduction"
    , Group "Development"
      [ Page "prerequisites.html"   "Prerequisites"
      , Page "build.html"           "Building"
      , Page "loading.html"         "Loading"
      ]
    , Group "Flight Software"
      [ Page "flight-overview.html" "Overview"
      ]
    ]
  langpages =
    [ Page "index.html"          "Overview"
    , Group "Ivory"
      [ Page "ivory-overview.html" "Language"
      , Page "fibwalkthrough.html" "Tutorial"
      ]
    , Group "Tower"
      [ Page "tower-overview.html" "Language"
      ]
    ]

navbar :: FilePath -> String
navbar currentpath = unlines $
  [ "<ul class=\"nav\"> "
  , entry "/index.html"           "Home"
  , entry "/languages/index.html" "Languages"
  , entry "/software/index.html"  "Software"
  , entry "/hardware/index.html"  "Hardware"
  , entry "/about.html"           "About"
  , "</ul>"
  ]
  where
  entry path desc =
    "<li" ++ (emphif path) ++ "><a href=\"" ++ path ++ "\">" ++
    desc ++ "</a></li> "
  emphif path = case currentpath == path of
    True  -> " class=\"active\" "
    False -> ""

templated :: Identifier -> Item String -> Compiler (Item String)
templated t input = loadAndApplyTemplate t ctx input >>= relativizeUrls
  where
  ctx :: Context String
  ctx = mconcat
    [ field "navbar"    $ \item -> return (navbar (itemFilePath item))
    , field "sidebar"   $ \item -> return (sidebarHTML sitemap item)
    , field "directory" $ \item -> return (itemDirectory item)
    , field "filepath"  $ \item -> return (itemFilePath item)
    , constField "copyright" "<p>&copy; Galois Inc. 2013</p>"
    , defaultContext
    ]

