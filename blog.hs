{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Control.Applicative (empty)
import Control.Monad (forM_, void)

import Text.Pandoc.Highlighting (styleToCss)
import Text.Pandoc.Options (ReaderOptions, WriterOptions(..))
import Hakyll

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

readerOptions :: ReaderOptions
readerOptions = defaultHakyllReaderOptions

writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions

markdownCompiler :: Compiler (Item String)
markdownCompiler = pandocCompilerWith readerOptions writerOptions

makeCss :: Rules ()
makeCss = do
  void $ match "static/css/*" $ do
      route $ stripPrefix "static/"
      compile compressCssCompiler
  forM_ (writerHighlightStyle writerOptions)
    (\ pandocCodeStyle ->
      create ["css/syntax.css"] $ do
          route idRoute
          compile $ do
              makeItem $ styleToCss pandocCodeStyle)

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

fullName :: String
fullName = "Etienne Millon"

authorField :: Context a
authorField = constField "author" fullName

renderPosts :: Tags -> Rules ()
renderPosts tags = do
  void $ match "posts/*" $ do
      route   $ setExtension ".html"
      compile $ do
        x1 <- markdownCompiler
        let ctx = mconcat [ dateField "date" "%B %e, %Y"
                          , tagsField "prettytags" tags
                          , authorField
                          , hnField
                          , defaultContext
                          ]
        x2 <- saveSnapshot "content" x1
        finalRenderer "templates/post.html" ctx x2

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
                              , authorField
                              , defaultContext
                              ]
            finalRenderer "templates/post.html" ctx x
    void $ match "drafts/*/*.mdwn" $ do
        route $ setExtension ".html"
        compile $
            markdownCompiler >>=
            loadAndApplyTemplate "templates/default.html" defaultContext >>=
            relativizeUrls

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
    , feedAuthorName  = fullName
    , feedAuthorEmail = "me@emillon.org"
    , feedRoot        = "http://blog.emillon.org"
    }
