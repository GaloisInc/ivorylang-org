
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
                     , pDesc :: String
                     }
              | Group { gDesc :: String
                      , gLeaves :: [PageTree]
                      }
              deriving (Eq, Show)


atDirectory :: FilePath -> PageTree -> Maybe PageTree
atDirectory dir t@(Tree p _ ls) =
  case (dir == p) of
    True -> Just t
    False -> msum $ map (recur p) ls 
  where
    prefix :: FilePath -> PageTree -> PageTree
    prefix x (Tree p d ls) = (Tree (x </> p) d ls)
    prefix x (Page p desc) = (Page (x </> p) desc)
    prefix x (Group d ls)  = (Group d (map (prefix x) ls))
    recur :: FilePath -> PageTree -> Maybe PageTree
    recur p subtree = atDirectory dir (prefix p subtree)

atDirectory _ (Page _ _) = Nothing
atDirectory _ (Group _ _) = Nothing

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
        l (Page p _) = p
ls _ = ""

tree :: PageTree -> String
tree pt = case pt of
  Tree d _ ps -> mktree d ps
  Page p _    -> p
  Group _ ps  -> mktree "" ps
  where
  mktree d ps = unlines $ concatMap (aux d) ps
  aux prefix (Tree d _ ps) = concatMap (aux (prefix </> d)) ps
  aux prefix (Page p _)    = [ prefix </> p ]
  aux prefix (Group _ ps)  = concatMap (aux prefix) ps


sidebarHTML :: PageTree -> Item a -> String
sidebarHTML sitemap item =
  fromMaybe "" $ listing <$> atDirectory pwd sitemap
  where
  pwd = itemDirectory item
  listing :: PageTree -> String
  listing (Tree p d xs) = unlines $
        [ "<div class=\"well sidebar-nav\">"
        , "  <ul class=\"nav nav-list\">"
        , "    <li class=\"nav-header\">" ++ d ++ "</li>"
        ] ++
        [ listitem p x | x <- xs ] ++
        [ "  </ul>"
        , "</div><!--/.well -->"
        ]
  listing _ = "" -- Should not be possible (result of atDirectory)
  listitem :: String -> PageTree -> String
  listitem p (Tree p' d _) =
    "<li><a href=\"" ++ fullpath ++ "\">" ++ d ++ "</a></li>"
    where fullpath = p </> p' ++ "/"
  listitem p (Page p' desc) =
    "<li" ++  emph ++ "><a href=\"" ++ fullpath ++ "\">" ++ desc ++
    "</a></li>"
    where
    fullpath = p </> p'
    emph = case (itemFilePath item) == fullpath of
             True ->  " class=\"active\" "
             False -> ""
  listitem p (Group d ps) = header ++ contents
    where
    header = "    <li class=\"nav-header\">" ++ d ++ "</li>\n"
    contents = unlines $ map (listitem p) ps

