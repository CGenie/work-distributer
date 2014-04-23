{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.List (sortBy, (\\))
import qualified Data.ConfigFile as ConfigFile
import Data.ByteString.Lazy.Char8 (pack)
import Data.Either.Utils (forceEither)
import Data.Digest.Pure.SHA (sha256, showDigest)
import System.Directory (getHomeDirectory, createDirectoryIfMissing, getDirectoryContents)
import qualified Data.Text as T

import Data.DateTime (getCurrentTime, DateTime)

import           Control.Applicative

import           Snap
import Snap.Snaplet.Heist
import Control.Lens
import Heist
import Heist.Interpreted


type SourceDir = String
type DestinationDir = String

type AppConfig = (SourceDir, DestinationDir)

type Hash = String

data SourceFileHashed = SourceFileHashed FilePath Hash
   deriving (Show, Eq)
fileFromHash :: SourceFileHashed -> FilePath
fileFromHash (SourceFileHashed fp _) = fp
hashFromHash :: SourceFileHashed -> Hash
hashFromHash (SourceFileHashed _ h) = h

data SourceFile = SourceFile SourceFileHashed DateTime
   deriving (Show, Eq)
hashFromSourceFile :: SourceFile -> SourceFileHashed
hashFromSourceFile (SourceFile sfh _) = sfh
sourceFileDateTime :: SourceFile -> DateTime
sourceFileDateTime (SourceFile _ dt) = dt

data AppState = AppState {
    _heist :: Snaplet (Heist AppState),

    _tst :: String,

    _stateSourceFiles :: [SourceFile],
    _stateFilesGiven :: [SourceFile],

    _stateSourceDir :: SourceDir,
    _stateTargetDir :: DestinationDir
}
makeLenses ''AppState

instance HasHeist AppState where
    heistLens = subSnaplet heist

main :: IO ()
main = do
    (_, site, _) <- runSnaplet Nothing distributerInit

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


distributerInit :: SnapletInit AppState AppState
distributerInit = makeSnaplet "distributer" "Work distributer" Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit "templates"

    (source, target) <- liftIO $ readConfig
    sourceFiles' <- liftIO $ getSourceFiles source
    sourceFilesHashed <- liftIO $ mapM computeFileHash sourceFiles'
    now <- liftIO $ getCurrentTime
    let sourceFiles = map (\sf -> SourceFile sf now) sourceFilesHashed

    liftIO $ putStrLn "Your watched files:"
    liftIO $ mapM_ (putStrLn . show) sourceFiles

    liftIO $ createDirectoryIfMissing True target

    let s = AppState {
        _heist = h,

        _tst = "",

        _stateSourceFiles = sourceFiles,
        _stateFilesGiven = [],

        _stateSourceDir = source,
        _stateTargetDir = target
    }

    addRoutes [("", workList),
            ("giveFile", giveFile)]

    return s


-- ROUTES
workList :: Handler AppState AppState ()
workList = do
    sf <- use stateSourceFiles
    renderWithSplices "list" $ allWorkItems sf


giveFile :: Handler AppState AppState ()
giveFile = do
    sfs <- use stateSourceFiles
    gfs <- use stateFilesGiven

    liftIO $ print $ show gfs
    tst' <- use tst
    liftIO $ print tst'

    let rem = sfs \\ gfs
    let remaining' =
            if rem == []
                then sfs
                else rem

    let sf = last $ sortBy (\a b -> compare (sourceFileDateTime a) (sourceFileDateTime b)) remaining'

    stateFilesGiven .= [sf]

    tst .= (show sf)

    tst'' <- use tst

    liftIO $ print tst''

    --put tst

    --set tst tst''

    writeText $ T.pack (show sf)


-- RENDERERS
allWorkItems :: [SourceFile] -> Splices (SnapletISplice AppState)
allWorkItems sf = "items" ## (mapSplices $ runChildrenWith . listItem) sf

listItem :: Monad m => SourceFile -> Splices (Splice m)
listItem sourceFile = do
    let sfh = hashFromSourceFile sourceFile
    let sf = fileFromHash sfh
    let sh = hashFromHash sfh
    "listItem" ## textSplice (T.pack sf)
    "listItemURL" ## textSplice $ T.pack $ "get/" ++ sh


--site :: Snap ()
--site =
--    ifTop (writeBS "hello world") <|>
--    route [ ("foo", writeBS "bar")
--          , ("echo/:echoparam", echoHandler)
--          ] -- <|>
--    --dir "static" (serveDirectory ".")

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
