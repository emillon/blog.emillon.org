{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Arrow
import Control.Monad
import Data.Monoid

import Hakyll

main :: IO ()
main =
  hakyllWith conf rules
    where
      conf = defaultConfiguration { deployCommand = s3deploy }
      s3deploy = "s3cmd --guess-mime-type -P sync _site/ s3://blog.emillon.org/"

rules :: Rules ()
rules = do
  makeCss
  copyStatic
  renderPosts
  renderPostsList
  makeIndex
  {-makeTags-}
  {-makeRss-}
  buildTemplates

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

getTags' = buildTags "tag/*" (fromCapture "tag/*")

renderPosts :: Rules ()
renderPosts = do
  void $ match "posts/*" $ do
      route   $ setExtension ".html"
      compile $ do
        x1 <- pandocCompiler
        tags <- getTags'
        let ctx = mconcat [ dateField "date" "%B %e, %Y"
                          , tagsField "prettytags" tags
                          , defaultContext
                          ]
        x2 <- loadAndApplyTemplate "templates/post.html"    ctx x1
        x3 <- loadAndApplyTemplate "templates/default.html" ctx x2
        relativizeUrls x3
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
      what <- getUnderlying
      let p0 = Item what "empty page"
      p1 <- loadAndApplyTemplate "templates/posts.html" ctx2 p0
      p2 <- loadAndApplyTemplate "templates/default.html" ctx2 p1
      relativizeUrls p2
-- TODO add postCtx

makeIndex :: Rules ()
makeIndex = void $ do
  create ["index.html"] $ do
    route idRoute
    compile $ do
        tags <- getTags'
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
        what <- getUnderlying
        let post = Item what "empty page"
        p1 <- loadAndApplyTemplate "templates/index.html" ctx2 post
        p2 <- loadAndApplyTemplate "templates/default.html" ctx2 p1
        relativizeUrls p2

{-makeTags :: Rules-}
{-makeTags = do-}
  {-void $ create "tags" $ do-}
      {-post <- loadAll ("posts/*" `mappend` isRaw) (\_ ps -> getTags ps :: Tags String)-}
      {-return post-}
  {--- Add a tag list compiler for every tag-}
  {-match "tags/*" $ route $ setExtension ".html"-}
  {-metaCompile $ require_ "tags"-}
            {->>> arr tagsMap-}
            {->>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))-}

{-makeRss :: Rules-}
{-makeRss = void $ do-}
  {--- Render RSS feed-}
  {-match "rss.xml" $ route idRoute-}
  {-create "rss.xml" $ dorequireAll_ ("posts/*" `mappend` isRaw)-}
                 {->>> mapCompiler (arr $ copyBodyToField "description")-}
                 {->>> renderRss feedConfiguration-}

  {-match "feeds/*" $ route $ setExtension ".xml"-}
  {-metaCompile $ require_ "tags"-}
            {->>> arr tagsMap-}
            {->>> mapCompiler (arr tagFeedId *** arr tagFeedRss)-}
    {-where-}
      {-tagFeedId t = parseIdentifier ("feeds/"++t)-}
      {-tagFeedRss ps = constA ps-}
                  {->>> mapCompiler (arr $ copyBodyToField "description")-}
                  {->>> renderRss feedConfiguration-}

buildTemplates :: Rules ()
buildTemplates =
  void $ match "templates/*" $ compile templateCompiler

renderTagCloud' :: Tags -> Compiler String
renderTagCloud' = renderTagCloud 100 120

tagIdentifier :: String -> Identifier
tagIdentifier = fromCapture "tags/*"

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@
--
addPostList :: Context String -> [Item String] -> Compiler String
addPostList ctx posts = do
  tpl <- loadBody "templates/postitem.html"
  let orderedPosts = reverse $ chronological $ posts
  applyTemplateList tpl ctx orderedPosts

makeTagList :: String
            -> [Item String]
            -> Compiler (Item String)
makeTagList tag posts = do
  let ctx1 =
          titleField ("Posts tagged &#8216;" ++ tag ++ "&#8217;") `mappend`
          constField "feed" ("/feeds/" ++ tag ++ ".xml")
-- TODO + autre ctx ?
  postsString <- addPostList ctx1 posts
  let ctx2 = constField "posts" postsString `mappend` ctx1
-- TODO facto ce ctx1/2
  let p0 = error "p0" -- TODO virer error
  p1 <- loadAndApplyTemplate "templates/posts.html" ctx2 p0
  p2 <- loadAndApplyTemplate "templates/default.html" ctx2 p1
  relativizeUrls p2

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Enter the void *"
    , feedDescription = "Yet another random hacker"
    , feedAuthorName  = "Etienne Millon"
    , feedAuthorEmail = "me@emillon.org"
    , feedRoot        = "http://blog.emillon.org"
    }
