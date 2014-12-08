{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DiagramCompiler(diagramCompiler) where

import Data.Binary
import Data.Typeable

import Control.Lens
import Diagrams.Backend.SVG
import Diagrams.Builder
import Diagrams.TwoD
import Diagrams.Prelude
import Hakyll
import Text.Blaze.Svg.Renderer.String

newtype DiagramFile = DiagramFile FilePath
    deriving (Binary, Typeable)

instance Writable DiagramFile where
    write dst (Item _ (DiagramFile src)) = do
        res <- compileDiagram src
        case res of
            OK _ svg -> writeFile dst $ renderSvg svg
            Skipped _ -> return ()
            ParseErr e -> error e
            InterpErr e -> error $ ppInterpError e

diagramCompiler :: Compiler (Item DiagramFile)
diagramCompiler = do
    ident <- getUnderlying
    makeItem $ DiagramFile $ toFilePath ident

compileDiagram :: FilePath -> IO (BuildResult SVG R2)
compileDiagram path = do
    src <- readFile path
    let bo0 = mkBuildOpts SVG zeroV (SVGOptions (mkSizeSpec xsize ysize) Nothing)
        bo = bo0 & imports .~ ["Diagrams.Backend.SVG"]
                 & diaExpr .~ src
    buildRes <- buildDiagram bo
    return buildRes
        where
            xsize = Just 400
            ysize = Just 300
