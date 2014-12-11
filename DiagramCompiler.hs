{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DiagramCompiler(diagramCompiler) where

import Control.Monad
import Data.Binary
import Data.List
import Data.Typeable
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Temp
import System.Posix.Files
import System.Process

import Control.Lens
import Diagrams.Backend.SVG
import Diagrams.TwoD
import Diagrams.Prelude
import Hakyll
import Text.Blaze.Svg.Renderer.String

newtype DiagramFile = DiagramFile FilePath
    deriving (Binary, Typeable)

instance Writable DiagramFile where
    write dst (Item _ (DiagramFile src)) = do
        compileDiagram src dst

diagramCompiler :: Compiler (Item DiagramFile)
diagramCompiler = do
    ident <- getUnderlying
    makeItem $ DiagramFile $ toFilePath ident

runCmd :: String -> [String] -> IO ()
runCmd cmd args = do
    (_, _, _, p) <- createProcess (proc cmd args) { std_out = Inherit, std_err = Inherit }
    exitCode <- waitForProcess p
    unless (exitCode == ExitSuccess) $ error $ "Command failed: " ++ unwords (cmd:args)

compileDiagram :: FilePath -> FilePath -> IO ()
compileDiagram src dest = do
    let temp = "diagram"
        xsize = 400
        ysize = 300
    args <- detectSandboxArgs
    runCmd "ghc" $ ["--make", src, "-o", temp] ++ args
    runCmd ("./" ++ temp)
        [ "-o", dest
        , "-w", show xsize
        , "-h", show ysize
        ]
    removeFile temp

detectSandboxArgs :: IO [String]
detectSandboxArgs = do
    sandboxDir <- detectSandbox
    case sandboxDir of
        Nothing -> return []
        Just d -> return ["-package-db", d]

detectSandbox :: IO (Maybe FilePath)
detectSandbox = do
    ex <- doesDirectoryExist ".cabal-sandbox"
    if ex
        then findMatching (".conf.d" `isSuffixOf`) ".cabal-sandbox"
        else return Nothing

findMatching :: (FilePath -> Bool) -> FilePath -> IO (Maybe FilePath)
findMatching ok base = do
    cs <- getDirectoryContents base
    let dopt = find ok cs
    return $ fmap (base </>) dopt
