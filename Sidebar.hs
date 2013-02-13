
module Sidebar where

import Hakyll
import System.FilePath
import Data.Maybe
import Data.String.Utils (replace)
import Control.Applicative
import Control.Monad

data PageTree = Tree { tPath    :: FilePath
                     , tDisp    :: String
                     , tLeaves  :: [PageTree]
                     }
              | Page { pPath :: String
                     , pDisp :: String
                     , pDesc :: String
                     }
              deriving (Eq, Show)


atDirectory :: FilePath -> PageTree -> Maybe PageTree
atDirectory dir t@(Tree p _ ls) =
  case (dir == p) of
    True -> Just t
    False -> msum $ map (recur p) ls 
  where
    prefix :: FilePath -> PageTree -> PageTree
    prefix x (Tree p d ls)   = (Tree (x </> p) d ls)
    prefix x (Page p d desc) = (Page (x </> p) d desc)
    recur :: FilePath -> PageTree -> Maybe PageTree
    recur p subtree = atDirectory dir (prefix p subtree)

atDirectory _ (Page _ _ _) = Nothing

itemDirectory :: Item a -> FilePath 
itemDirectory = dropTrailingPathSeparator
              . dropFileName
              . itemFilePath

itemFilePath :: Item a -> FilePath 
itemFilePath  = (flip addExtension) "html"
              . dropExtension
              . replace "pages" ""  -- XXX get rid of leading, 
                                    -- yes i know this is evil. 
              . toFilePath
              . itemIdentifier

ls :: PageTree -> String
ls (Tree _ _ ps) = unlines $ map l ps
  where l (Tree p _ _) = p ++ "/"
        l (Page p _ _) = p
ls _ = ""

tree :: PageTree -> String
tree (Tree d _ ps) = unlines $ concatMap (aux d) ps
  where
  aux prefix (Tree d _ ps) = concatMap (aux (prefix </> d)) ps
  aux prefix (Page p _ _) = [ prefix </> p ]
tree (Page p _ _) = p


sidebarHTML :: PageTree -> Item a -> String
sidebarHTML sitemap item =
  fromMaybe "" $ listing <$> atDirectory pwd sitemap
  where
  pwd = itemDirectory item
  listing :: PageTree -> String
  listing (Page _ _ _)  = "" -- Should not be possible (result of atDirectory)
  listing (Tree p d xs) = unlines $
        [ "<div class=\"well sidebar-nav\">"
        , "  <ul class=\"nav nav-list\">"
        , "    <li class=\"nav-header\">" ++ d ++ "</li>"
        ] ++
        [ listitem x | x <- xs ] ++
        [ "  </ul>"
        , "</div><!--/.well -->"
        ]
    where
    listitem :: PageTree -> String
    listitem (Tree p' d _) = let fullpath = p </> p' ++ "/" in
      "<li><a href=\"" ++ fullpath ++ "\">" ++ d ++ "</a></li>"

    listitem (Page p' _ desc) =
      "<li" ++  emph ++ "><a href=\"" ++ fullpath ++ "\">" ++ desc ++
      "</a></li>"
      where
      fullpath = p </> p'
      emph = case (itemFilePath item) == fullpath of
               True ->  " class=\"active\" "
               False -> ""
  
    
  

