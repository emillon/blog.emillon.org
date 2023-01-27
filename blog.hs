{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer
import Data.Monoid
import Data.String (fromString)
import Data.Text (replace, Text)
import System.FilePath

import Hakyll
import Text.Pandoc.Definition
import Text.Pandoc.Walk

main :: IO ()
main = hakyll rules

rules :: Rules ()
rules = do
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")
  buildTemplates
  makeCss
  copyStatic
  renderPosts tags
  renderPostsList
  makeIndex tags
  makeTags tags
  makeRss
  makeDrafts tags

stripPrefix :: String -> Routes
stripPrefix pfx = gsubRoute pfx (const "")

makeCss :: Rules ()
makeCss =
  void $ match "static/css/*" $ do
      route $ stripPrefix "static/"
      compile compressCssCompiler

copyStatic :: Rules ()
copyStatic =
  void $ match "static/**" $ do
      route $ stripPrefix "static/"
      compile copyFileCompiler

finalRenderer :: Identifier -> Context String -> Item String
              -> Compiler (Item String)
finalRenderer tplPath ctx x1 = do
  x2 <- loadAndApplyTemplate tplPath ctx x1
  x3 <- loadAndApplyTemplate "templates/default.html" ctx x2
  relativizeUrls x3

metaField :: String -> Compiler String -> Context a
metaField f d =
    field f $ \ i -> do
        value <- getMetadataField (itemIdentifier i) f
        maybe d return value

hnField :: Context a
hnField = metaField "hn" empty

renderPosts :: Tags -> Rules ()
renderPosts tags = do
  void $ match "posts/*" $ do
      route   $ setExtension ".html"
      compile $ do
        x1 <- markdownCompiler
        let ctx = mconcat [ dateField "date" "%B %e, %Y"
                          , tagsField "prettytags" tags
                          , hnField
                          , defaultContext
                          ]
        x2 <- saveSnapshot "content" x1
        finalRenderer "templates/post.html" ctx x2
  copyAssets "posts"

renderPostsList :: Rules ()
renderPostsList = void $ do
  create ["posts.html"] $ do
    route idRoute
    compile $ do
      posts :: [Item String] <- loadAll "posts/*"
      ctx <- makePostsContext "All posts" posts [("feed", "/rss.xml")]
      p0 <- makeItem ""
      finalRenderer "templates/posts.html" ctx p0

makePostsContext :: String
                 -> [Item String]
                 -> [(String, String)]
                 -> Compiler (Context String)
makePostsContext title posts fields = do
  let ctx1 = mconcat $ [ constField k v | (k, v) <- fields ]
                    ++ [ dateField "date" "%d %b %Y"
                       , defaultContext
                       ]
  postsString <- addPostList ctx1 posts
  return $ mconcat [ constField "title" title
                   , constField "posts" postsString
                   , ctx1
                   ]

makeIndex :: Tags -> Rules ()
makeIndex tags = void $ do
  create ["index.html"] $ do
    route idRoute
    compile $ do
        allPosts :: [Item String] <- loadAll "posts/*"
        recentPosts <- recentFirst $ allPosts
        let posts = take 3 recentPosts
        tagCloud <- renderTagCloud' tags
        ctx <- makePostsContext "Home" posts [("tagcloud", tagCloud)]
        post <- makeItem ""
        finalRenderer "templates/index.html" ctx post

makeTags :: Tags -> Rules ()
makeTags tags =
  tagsRules tags $ \ tag pattern -> do
    let feedPath = "feeds/" ++ tag ++ ".xml"
    route idRoute
    compile $ do
      x1 <- makeItem ""
      posts <- loadAll pattern
      ctx <- makePostsContext ("Posts tagged &#8216;" ++ tag ++ "&#8217;")
                               posts
                               [("feed", "/" ++ feedPath)]
      finalRenderer "templates/posts.html" ctx x1
    version "rss" $ do
      route $ constRoute feedPath
      compile $ do
        posts <- loadAllSnapshots pattern "content"
        rssFromPosts posts

rssFromPosts :: [Item String] -> Compiler (Item String)
rssFromPosts posts = do
  let ctx = mconcat [ bodyField "description"
                    , defaultContext
                    ]
  renderRss feedConfiguration ctx posts

makeRss :: Rules ()
makeRss = void $
  create ["rss.xml"] $ do
    route idRoute
    compile $ do
      posts <- loadAllSnapshots "posts/*" "content"
      rssFromPosts posts

makeDrafts :: Tags -> Rules ()
makeDrafts tags = do
    void $ match "drafts/*" $ do
        route $ setExtension ".html"
        compile $ do
            x <- markdownCompiler
            let ctx = mconcat [ constField "date" "No date"
                              , tagsField "prettytags" tags
                              , defaultContext
                              ]
            finalRenderer "templates/post.html" ctx x
    void $ match "drafts/*/*.mdwn" $ do
        route $ setExtension ".html"
        compile $
            markdownCompiler >>=
            loadAndApplyTemplate "templates/default.html" defaultContext >>=
            relativizeUrls
    copyAssets "drafts"

copyAssets :: String -> Rules ()
copyAssets base =
    void $ match pattern $ do
        route idRoute
        compile copyFileCompiler
    where
        pattern = foldr1 (.||.) $ map makePat extensions
        makePat ext = fromGlob $ base ++ "/*/*." ++ ext
        extensions = ["gif", "jpg", "png"]

buildTemplates :: Rules ()
buildTemplates =
  void $ match "templates/*" $ compile templateCompiler

renderTagCloud' :: Tags -> Compiler String
renderTagCloud' = renderTagCloud 70 160

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@
--
addPostList :: Context String -> [Item String] -> Compiler String
addPostList ctx posts = do
  tpl <- loadBody "templates/postitem.html"
  orderedPosts <- recentFirst posts
  applyTemplateList tpl ctx orderedPosts

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Enter the void *"
    , feedDescription = "Yet another random hacker"
    , feedAuthorName  = "Etienne Millon"
    , feedAuthorEmail = "me@emillon.org"
    , feedRoot        = "http://blog.emillon.org"
    }

markdownCompiler :: Compiler (Item String)
markdownCompiler =
    pandocCompilerWithTransformM defaultHakyllReaderOptions defaultHakyllWriterOptions replaceThis

getAssetDirectory :: Compiler FilePath
getAssetDirectory =
    takeBaseName . toFilePath <$> getUnderlying

replaceThisTarget :: Text -> Target -> Target
replaceThisTarget s (url, title) = (newUrl, title)
    where
        newUrl = replace "THIS" s url

-- replace "THIS" in urls by the current article.
replaceThis :: Pandoc -> Compiler Pandoc
replaceThis pdc = do
    dir <- fromString <$> getAssetDirectory
    return $ walk (replaceThisTarget dir) pdc

walkFromWalkM :: (forall m . (Monad m, Functor m) => (a -> m a) -> b -> m b)
              -> (a -> a) -> b -> b
walkFromWalkM wm f b =
    runIdentity $ wm (Identity . f) b

queryFromWalkM :: (Monoid c)
               => (forall m . (Monad m, Functor m) => (a -> m a) -> b -> m b)
               -> (a -> c) -> b -> c
queryFromWalkM wm f b = do
    let go a = do
            tell (f a)
            return a
    execWriter $ wm go b

instance Walkable Target Inline where
    walkM _ (Str s) = return $ Str s
    walkM f (Emph is) = Emph <$> mapM (walkM f) is
    walkM f (Strong is) = Strong <$> mapM (walkM f) is
    walkM f (Strikeout is) = Strikeout <$> mapM (walkM f) is
    walkM f (Superscript is) = Superscript <$> mapM (walkM f) is
    walkM f (Subscript is) = Subscript <$> mapM (walkM f) is
    walkM f (SmallCaps is) = SmallCaps <$> mapM (walkM f) is
    walkM f (Quoted qt is) = Quoted qt <$> mapM (walkM f) is
    walkM f (Cite cs is) = liftM2 Cite (mapM (walkM f) cs) (mapM (walkM f) is)
    walkM _ (Code a s) = return $ Code a s
    walkM _ Space = return Space
    walkM _ LineBreak = return LineBreak
    walkM _ (Math mt s) = return $ Math mt s
    walkM _ (RawInline fmt s) = return $ RawInline fmt s
    walkM f (Link attr is tgt) = liftM2 (Link attr) (mapM (walkM f) is) (f tgt)
    walkM f (Image attr is tgt) = liftM2 (Image attr) (mapM (walkM f) is) (f tgt)
    walkM f (Note bs) = Note <$> mapM (walkM f) bs
    walkM f (Span a is) = Span a <$> mapM (walkM f) is
    walkM f SoftBreak = return SoftBreak

    walk = walkFromWalkM walkM
    query = queryFromWalkM walkM

instance Walkable Target Block where
    walkM f = walkM $ \ (x :: Inline) -> walkM f x
    walk = walkFromWalkM walkM
    query = queryFromWalkM walkM

instance Walkable Target Citation where
    walkM f = walkM $ \ (x :: Inline) -> walkM f x
    walk = walkFromWalkM walkM
    query = queryFromWalkM walkM

instance Walkable Target Pandoc where
    walkM f = walkM $ \ (x :: Inline) -> walkM f x
    walk = walkFromWalkM walkM
    query = queryFromWalkM walkM
