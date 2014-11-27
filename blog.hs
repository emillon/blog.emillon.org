{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad
import Data.Monoid

import Hakyll

import AssetDirectory

main :: IO ()
main =
  hakyllWith conf rules
    where
      conf = defaultConfiguration { deployCommand = s3deploy }
      s3deploy = "s3cmd --guess-mime-type -P sync _site/ s3://blog.emillon.org/"

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

renderPosts :: Tags -> Rules ()
renderPosts tags = do
  void $ match "posts/*" $ do
      route   $ setExtension ".html"
      compile $ do
        x1 <- markdownCompiler
        let ctx = mconcat [ dateField "date" "%B %e, %Y"
                          , tagsField "prettytags" tags
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
        extensions = ["gif", "jpg"]

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
