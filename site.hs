--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Data.Monoid         (mappend, mconcat)
import           System.FilePath     (dropFileName, dropExtension, (<.>), (</>))
import           Hakyll

import           Sidebar
--------------------------------------------------------------------------------

-- Sitemap is used to generate the navbars
sitemap :: PageTree
sitemap = Tree "/" "Home"
    [ Page "index.html"          "Overview"
    , Group "Ivory Language"
      [ Page "ivory-introduction.html" "Introduction"
      , Page "ivory-concepts.html"     "Concepts"
      , Page "ivory-cheatsheet.html"   "Cheatsheet"
      , Page "ivory-reference.html"    "Reference"
      , Page "ivory-tools.html"        "Toolchain"
      , Page "ivory-libs.html"         "Libraries"
      , Page "ivory-fib.html"          "Tutorial"
      ]
    , Group "Tower Language"
      [ Page "tower-overview.html" "Introduction"
      ]
    ]

standardPandocPagesSubdir d = do
    match (fromGlob ("pages/" ++ d ++ "*.md")) $ do
        route   $ gsubRoute "pages/"  (const "") `composeRoutes`
                  setExtension "html"
        compile $ pandocCompiler >>= (templated "templates/standard.html")

config :: Configuration
config = defaultConfiguration { deployCommand = deploy }
  where
  path = "/srv/www/ivorylang.org/public_html/"
  server = "alfred.galois.com"
  deploy = "rsync -crlOe ssh --delete _site/* " ++ server ++ ":" ++ path
    -- rsync might set the group wrong, we need to change all of the items
    -- we own to group smaccm, supressing errors for the items we do not own
    ++ " && ssh " ++ server ++  " chgrp -R -f smaccm "  ++  path
    ++ " ;  ssh " ++ server ++  " chmod -R -f g+w "  ++  path
    ++ " ;  exit 0"

main :: IO ()
main = hakyllWith config $ do

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "artifacts/*" $ do
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

    match "templates/*" $ compile templateCompiler

    standardPandocPagesSubdir ""


navbar :: FilePath -> String
navbar currentpath = unlines $
  [ "<ul class=\"nav\"> "
  , entry "/index.html"           "Home"
  , entry "/about.html"           "About"
  , "</ul>"
  ]
  where
  entry path desc =
    "<li" ++ (emphif path) ++ "><a href=\"" ++ path ++ "\">" ++
    desc ++ "</a></li> "
  emphif path = if under path then " class=\"active\" " else ""
  under path | rootdir path = path == currentpath
             | otherwise    = dropFileName path == dropFileName currentpath
  rootdir path = dropFileName path == "/"

templated :: Identifier -> Item String -> Compiler (Item String)
templated t input = loadAndApplyTemplate t ctx input >>= relativizeUrls
  where
  ctx :: Context String
  ctx = mconcat
    [ field "navbar"    $ \item -> return (navbar (itemFilePath item))
    , field "sidebar"   $ \item -> return (sidebarHTML sitemap item)
    , field "directory" $ \item -> return (itemDirectory item)
    , field "filepath"  $ \item -> return (itemFilePath item)
    , constField "copyright" "<p>&copy; Galois Inc. 2014-2017</p>"
    , field "markdownfile"  $ \item -> return (markdownFile item)
    , defaultContext
    ]

markdownFile :: Item a -> String
markdownFile item = (dropExtension (itemFilePath item)) <.> "md"

