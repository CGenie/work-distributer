{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ConfigFile as ConfigFile
import Data.ByteString.Lazy.Char8 (pack)
import Data.Either.Utils (forceEither)
import Data.Digest.Pure.SHA (sha256, showDigest)
import System.Directory (getHomeDirectory, createDirectoryIfMissing, getDirectoryContents)

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server

type SourceDir = String
type DestinationDir = String
type Hash = String
data SourceFileHashed = SourceFileHashed (FilePath, Hash)
   deriving (Show, Eq)

main :: IO ()
main = do
    (source, target) <- readConfig
    sourceFiles <- getSourceFiles source
    sourceFilesHashed <- mapM computeFileHash sourceFiles

    putStrLn "Your watched files:"
    mapM_ (putStrLn . show) sourceFilesHashed

    createDirectoryIfMissing True target

    quickHttpServe site

readConfig :: IO (SourceDir, DestinationDir)
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
        in SourceFileHashed (filePath, hash)

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
