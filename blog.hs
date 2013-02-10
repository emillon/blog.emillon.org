{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad
import Data.Monoid
import System.FilePath

import Hakyll

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

stripPrefix :: String -> Routes
stripPrefix pfx = gsubRoute pfx (const "")

makeCss :: Rules ()
makeCss =
  void $ match "static/css/*" $ do
      route $ stripPrefix "static/"
      compile compressCssCompiler

copyStatic :: Rules ()
copyStatic =
  void $ match "static/*" $ do
      route $ stripPrefix "static/"
      compile copyFileCompiler

isRaw, isNotRaw :: Pattern
isRaw = hasVersion "raw"
isNotRaw = hasNoVersion

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
        x1 <- pandocCompiler
        let ctx = mconcat [ dateField "date" "%B %e, %Y"
                          , tagsField "prettytags" tags
                          , defaultContext
                          ]
        finalRenderer "templates/post.html" ctx x1
  void $ version "raw" $
    match "posts/*" $ do
      compile $ pandocCompiler
            >>= relativizeUrls

renderPostsList :: Rules ()
renderPostsList = void $ do
  create ["posts.html"] $ do
    route idRoute
    compile $ do
      posts :: [Item String] <- loadAll ("posts/*" .&&. isNotRaw)
      let ctx1 = mconcat [ titleField "All posts"
                         , constField "feed" "/rss.xml"
                         , dateField "date" "%B %e, %Y"
                         , defaultContext
                         ]
      postsString <- addPostList ctx1 posts
      let ctx2 = constField "posts" postsString `mappend` ctx1
      p0 <- makeItem ""
      finalRenderer "templates/posts.html" ctx2 p0

makeIndex :: Tags -> Rules ()
makeIndex tags = void $ do
  create ["index.html"] $ do
    route idRoute
    compile $ do
        tagCloud <- renderTagCloud' tags
        let ctx1 = mconcat [ titleField "Home"
                           , constField "tagcloud" tagCloud
                           , dateField "date" "%B %e, %Y"
                           , defaultContext
                           ]
        allPosts :: [Item String] <- loadAll ("posts/*" .&&. isNotRaw)
        let posts = take 3 . reverse . chronological $ allPosts
        postsString <- addPostList ctx1 posts
        let ctx2 = constField "posts" postsString `mappend` ctx1
        post <- makeItem ""
        finalRenderer "templates/index.html" ctx2 post

makeTags :: Tags -> Rules ()
makeTags tags =
  tagsRules tags $ \ tag pattern -> do
    let feedPath = "feeds/" ++ tag ++ ".xml"
    route idRoute
    compile $ do
      x1 <- makeItem ""
      posts <- loadAll pattern
      let ctx1 = mconcat [ constField "feed" $ "/" ++ feedPath
                         , dateField "date" "%B %e, %Y"
                         , defaultContext
                         ]
      postsString <- addPostList ctx1 posts
      let ctx2 = mconcat [ constField "posts" postsString
                         , constField "title" $ "Posts tagged &#8216;" ++ tag ++ "&#8217;"
                         , ctx1
                         ]
      finalRenderer "templates/posts.html" ctx2 x1
    version "rss" $ do
      route $ constRoute feedPath
      compile $ do
        posts <- loadAll pattern
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
      posts <- loadAll ("posts/*" .&&. isRaw)
      rssFromPosts posts

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
  let orderedPosts = reverse $ chronological $ posts
  applyTemplateList tpl ctx orderedPosts

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Enter the void *"
    , feedDescription = "Yet another random hacker"
    , feedAuthorName  = "Etienne Millon"
    , feedAuthorEmail = "me@emillon.org"
    , feedRoot        = "http://blog.emillon.org"
    }
