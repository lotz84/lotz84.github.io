{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Debug.Trace
import Hakyll

main :: IO ()
main = hakyll $ do
    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "about.html" $ do
        route   $ idRoute
        compile $ do
            getResourceBody
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    create ["posts/index.html"] $ do
        route   idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" `mappend` complement "posts/index.html")
            let postsCtx = listField "posts" (shortUrlField `mappend` postCtx) (return posts)
                    `mappend` constField "title" "filp map"
                    `mappend` defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/post-list.html" postsCtx
                >>= loadAndApplyTemplate "templates/default.html" postsCtx
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        let postsCtx = shortUrlField `mappend` postCtx
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postsCtx
            >>= loadAndApplyTemplate "templates/default.html" postsCtx
            >>= relativizeUrls

    match "index.html" $ do
        route   idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" `mappend` complement "posts/index.html")
            let indexCtx = listField "posts" (shortUrlField `mappend` postCtx) (return posts) `mappend` defaultContext
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
    create ["feed.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` constField "description" ""
            posts <- fmap (take 10) . recentFirst =<< loadAll ("posts/*" `mappend` complement "posts/index.html")
            renderRss myFeedConfiguration feedCtx posts

    match "templates/*" $ compile templateCompiler

postCtx :: Context String
postCtx = dateField "date" "%Y/%m/%d" `mappend` defaultContext

shortUrlField :: Context a
shortUrlField = field "surl" $
    fmap (maybe "" (reverse . tail . snd . break ('.'==) . reverse . toUrl)) . getRoute . itemIdentifier

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "flip map: latest posts"
    , feedDescription = ""
    , feedAuthorName  = "Tatsuya Hirose"
    , feedAuthorEmail = ""
    , feedRoot        = "http://lotz84.github.io/posts/"
    }
