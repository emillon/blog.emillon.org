{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module AssetDirectory ( getAssetDirectory
                      , replaceThis
                      ) where

import Control.Applicative
import Data.Monoid
import System.FilePath

import Control.Monad.Identity
import Control.Monad.Writer
import Data.List.Utils
import Hakyll
import Text.Pandoc.Definition
import Text.Pandoc.Walk

getAssetDirectory :: Compiler FilePath
getAssetDirectory =
    takeBaseName . toFilePath <$> getUnderlying

replaceThisTarget :: String -> Target -> Target
replaceThisTarget s (url, title) = (newUrl, title)
    where
        newUrl = replace "THIS" s url

-- replace "THIS" in urls by the current article.
replaceThis :: Pandoc -> Compiler Pandoc
replaceThis pdc = do
    dir <- getAssetDirectory
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
    walkM f (Link is tgt) = liftM2 Link (mapM (walkM f) is) (f tgt)
    walkM f (Image is tgt) = liftM2 Image (mapM (walkM f) is) (f tgt)
    walkM f (Note bs) = Note <$> mapM (walkM f) bs
    walkM f (Span a is) = Span a <$> mapM (walkM f) is

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
