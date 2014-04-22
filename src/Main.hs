{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List (sortBy)
import qualified Data.ConfigFile as ConfigFile
import Data.ByteString.Lazy.Char8 (pack)
import Data.Either.Utils (forceEither)
import Data.Digest.Pure.SHA (sha256, showDigest)
import System.Directory (getHomeDirectory, createDirectoryIfMissing, getDirectoryContents)

import Data.DateTime (getCurrentTime, DateTime)

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server

type SourceDir = String
type DestinationDir = String

type AppConfig = (SourceDir, DestinationDir)

type Hash = String

data SourceFileHashed = SourceFileHashed FilePath Hash
   deriving (Show, Eq)

data SourceFile = SourceFile SourceFileHashed DateTime
   deriving (Show, Eq)

data AppState = AppState {
    stateSourceFiles :: [SourceFile],
    stateSourceDir :: SourceDir,
    stateTargetDir :: DestinationDir
} deriving (Show)

main :: IO ()
main = do
    (source, target) <- readConfig
    sourceFiles' <- getSourceFiles source
    sourceFilesHashed <- mapM computeFileHash sourceFiles'
    now <- getCurrentTime
    let sourceFiles = map (\sf -> SourceFile sf now) sourceFilesHashed

    putStrLn "Your watched files:"
    mapM_ (putStrLn . show) sourceFiles

    createDirectoryIfMissing True target

    let state = AppState {stateSourceFiles = sourceFiles, stateSourceDir = source, stateTargetDir = target}

    quickHttpServe site

readConfig :: IO AppConfig
readConfig = do
    home <- getHomeDirectory
    val <- ConfigFile.readfile ConfigFile.emptyCP (home ++ "/.work-distributer")
    let cp = forceEither val
    let source = forceEither $ ConfigFile.get cp "directories" "source"
    let target = forceEither $ ConfigFile.get cp "directories" "target"

    putStrLn $ "Source dir: " ++ source
    putStrLn $ "Target dir: " ++ target

    return (source, target)

getSourceFiles :: SourceDir -> IO [FilePath]
getSourceFiles source = do
    createDirectoryIfMissing True source
    allFiles <- getDirectoryContents source

    let sourceFiles = filter (flip notElem [".", ".."]) allFiles
    let sourceFilesWithDir = map (\f -> source ++ "/" ++ f) sourceFiles

    return sourceFilesWithDir

computeFileHash :: FilePath -> IO SourceFileHashed
computeFileHash filePath = do
    contents <- readFile filePath

    return $ computeFileContentHash filePath contents

computeFileContentHash :: FilePath -> String -> SourceFileHashed
computeFileContentHash filePath contents = 
        let hash = showDigest $ sha256 $ pack $ filePath ++ contents
        in SourceFileHashed filePath hash

site :: Snap ()
site =
    ifTop (writeBS "hello world") <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          ] <|>
    dir "static" (serveDirectory ".")

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param


serveFile :: [SourceFile] -> Snap (Maybe SourceFile)
serveFile [] = return Nothing
serveFile sourceFiles = do
    let sf = last $ sortBy (\a b -> compare (sourceFileDateTime a) (sourceFileDateTime b)) sourceFiles
    return $ Just sf


sourceFileDateTime :: SourceFile -> DateTime
sourceFileDateTime (SourceFile _ dt) = dt
