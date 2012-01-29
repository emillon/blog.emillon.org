{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow
import Control.Monad
import Data.Monoid

import Hakyll

main :: IO ()
main =
  hakyllWith conf rules
    where
      conf = defaultHakyllConfiguration { deployCommand = s3deploy }
      s3deploy = "s3cmd -P sync _site/ s3://blog.emillon.org/"

rules :: Rules
rules = do
  makeCss
  renderPosts
  renderPostsList
  makeIndex
  makeTags
  makeRss
  buildTemplates

makeCss :: Rules
makeCss =
  void $ match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

pageComp = pageCompiler
       >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
       >>> renderTagsField "prettytags" (fromCapture "tags/*")

isRaw, isNotRaw :: Pattern a
isRaw = inGroup $ Just "raw"
isNotRaw = inGroup Nothing

renderPosts :: Rules
renderPosts = do
  void $ match "posts/*" $ do
      route   $ setExtension ".html"
      compile $ pageComp
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler
  void $ group "raw" $
    match "posts/*" $ do
      route   $ setExtension ".html"
      compile $ pageComp
            >>> relativizeUrlsCompiler

renderPostsList :: Rules
renderPostsList = void $ do
  match "posts.html" $ route idRoute
  create "posts.html"
      $ constA mempty
    >>> arr (setField "title" "All posts")
    >>> requireAllA ("posts/*" `mappend` isNotRaw) addPostList
    >>> applyTemplateCompiler "templates/posts.html"
    >>> applyTemplateCompiler "templates/default.html"
    >>> relativizeUrlsCompiler

makeIndex :: Rules
makeIndex = void $ do
  match "index.html" $ route idRoute
  create "index.html"
      $ constA mempty
    >>> arr (setField "title" "Home")
    >>> requireA "tags" (setFieldA "tagcloud" renderTagCloud')
    >>> requireAllA ("posts/*" `mappend` isNotRaw) (second (arr (take 3 . reverse . chronological)) >>> addPostList)
    >>> applyTemplateCompiler "templates/index.html"
    >>> applyTemplateCompiler "templates/default.html"
    >>> relativizeUrlsCompiler

makeTags :: Rules
makeTags = do
  void $ create "tags" $
      requireAll ("posts/*" `mappend` isRaw) (\_ ps -> readTags ps :: Tags String)
  -- Add a tag list compiler for every tag
  match "tags/*" $ route $ setExtension ".html"
  metaCompile $ require_ "tags"
            >>> arr tagsMap
            >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))

makeRss :: Rules
makeRss = void $ do
  -- Render RSS feed
  match "rss.xml" $ route idRoute
  create "rss.xml" $ requireAll_ ("posts/*" `mappend` isRaw)
                 >>> mapCompiler (arr $ copyBodyToField "description")
                 >>> renderRss feedConfiguration

  match "feeds/*" $ route $ setExtension ".xml"
  metaCompile $ require_ "tags"
            >>> arr tagsMap
            >>> mapCompiler (arr tagFeedId *** arr tagFeedRss)
    where
      tagFeedId t = parseIdentifier ("feeds/"++t)
      tagFeedRss ps = constA ps
                  >>> mapCompiler (arr $ copyBodyToField "description")
                  >>> renderRss feedConfiguration

buildTemplates :: Rules
buildTemplates =
  void $ match "templates/*" $ compile templateCompiler

renderTagCloud' :: Compiler (Tags String) String
renderTagCloud' = renderTagCloud tagIdentifier 100 120

tagIdentifier :: String -> Identifier (Page String)
tagIdentifier = fromCapture "tags/*"

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@
--
addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr (reverse . chronological)
        >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

makeTagList :: String
            -> [Page String]
            -> Compiler () (Page String)
makeTagList tag posts =
    constA (mempty, posts)
        >>> addPostList
        >>> arr (setField "title" ("Posts tagged &#8216;" ++ tag ++ "&#8217;"))
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Enter the void *"
    , feedDescription = "Yet another random hacker"
    , feedAuthorName  = "Etienne Millon"
    , feedRoot        = "http://blog.emillon.org"
    }
