{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Reader (ask)

import Data.List (sortBy, (\\))
import qualified Data.ConfigFile as ConfigFile
import Data.ByteString.Lazy.Char8 (pack)
import Data.Either.Utils (forceEither)
import Data.Digest.Pure.SHA (sha256, showDigest)
import System.Directory (getHomeDirectory, createDirectoryIfMissing, getDirectoryContents)
import qualified Data.Text as T

import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable (Typeable)
import Data.DateTime (getCurrentTime, DateTime)

import           Control.Applicative

import           Snap
import Snap.Snaplet.Heist
import Snap.Snaplet.AcidState
import Control.Lens
import Heist
import Heist.Interpreted


type SourceDir = String
type DestinationDir = String

type AppConfig = (SourceDir, DestinationDir)

type Hash = String

data SourceFile = SourceFile {
    _sourceFilePath :: FilePath,
    _sourceFileHash :: Hash,
    _sourceFileTimestamp :: DateTime
} deriving (Show, Eq, Ord, Typeable)
makeLenses ''SourceFile

deriveSafeCopy 0 'base ''SourceFile

data AppState = AppState {
    _tst :: String,

    _stateSourceFiles :: [SourceFile],
    _stateFilesGiven :: [SourceFile],

    _stateSourceDir :: SourceDir,
    _stateTargetDir :: DestinationDir
} deriving (Show, Ord, Eq, Typeable)
makeLenses ''AppState

deriveSafeCopy 0 'base ''AppState

readState :: Query AppState AppState
readState = do
        state <- ask
        return state

writeState :: AppState -> Update AppState ()
writeState newState = put newState

makeAcidic ''AppState ['writeState, 'readState]

data App = App {
    _heist :: Snaplet (Heist App),
    _acid :: Snaplet (Acid AppState)
}
makeLenses ''App

instance HasAcid App AppState where
    getAcidStore = view (acid.snapletValue)

instance HasHeist App where
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

computeFileHash :: FilePath -> IO SourceFile
computeFileHash filePath = do
    now <- liftIO $ getCurrentTime
    contents <- readFile filePath

    return $ SourceFile {
        _sourceFilePath = filePath,
        _sourceFileHash = computeFileContentHash filePath contents,
        _sourceFileTimestamp = now
    }

computeFileContentHash :: FilePath -> String -> Hash
computeFileContentHash filePath contents =
                        showDigest $ sha256 $ pack $ filePath ++ contents


distributerInit :: SnapletInit App App
distributerInit = makeSnaplet "distributer" "Work distributer" Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit "templates"

    (source, target) <- liftIO $ readConfig
    sourceFiles' <- liftIO $ getSourceFiles source
    sourceFiles <- liftIO $ mapM computeFileHash sourceFiles'

    liftIO $ putStrLn "Your watched files:"
    liftIO $ mapM_ (putStrLn . show) sourceFiles

    liftIO $ createDirectoryIfMissing True target

    let s = AppState{
        _tst = "",

        _stateSourceFiles = sourceFiles,
        _stateFilesGiven = [],

        _stateSourceDir = source,
        _stateTargetDir = target
    }

    a <- nestSnaplet "acid" acid $ acidInit s

    let app = App {
        _heist = h,
        _acid = a
    }

    addRoutes [("", workList),
            ("giveFile", giveFile)]

    return app


-- ROUTES
workList :: Handler App App ()
workList = do
    s <- query ReadState
    let sf = s ^. stateSourceFiles

    renderWithSplices "list" $ allWorkItems sf


giveFile :: Handler App App ()
giveFile = do
    now <- liftIO $ getCurrentTime

    s <- query ReadState
    let sfs = s  ^. stateSourceFiles
    let gfs = s ^. stateFilesGiven

    liftIO $ print $ show gfs

    let sfsHashes = map (\l -> l ^. sourceFileHash) sfs
    let gfsHashes = map (\l -> l ^. sourceFileHash) gfs

    let rem' = sfsHashes \\ gfsHashes
    let rem = filter (\l -> (l ^. sourceFileHash) `elem` rem') sfs
    let remaining' =
            if rem == []
                then sfs
                else rem

    let sf = head $ sortBy (\a b -> compare (a ^. sourceFileTimestamp) (b ^. sourceFileTimestamp)) remaining'

    let sf' = sourceFileTimestamp .~ now $ sf

    let s' =
            if rem == []
                then stateFilesGiven .~ [sf'] $ s
                else stateFilesGiven <>~ [sf'] $ s

    update $ WriteState s'

    writeText $ T.pack (show sf)


-- RENDERERS
allWorkItems :: [SourceFile] -> Splices (SnapletISplice App)
allWorkItems sfs = "items" ## (mapSplices $ runChildrenWith . listItem) sfs

listItem :: Monad m => SourceFile -> Splices (Splice m)
listItem sourceFile = do
    let sfh = sourceFile ^. sourceFileHash
    let sf =  sourceFile ^. sourceFilePath
    "listItem" ## textSplice (T.pack sf)
    "listItemURL" ## textSplice $ T.pack $ "get/" ++ sfh


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
    let sf = last $ sortBy (\a b -> compare (a ^. sourceFileTimestamp) (b ^. sourceFileTimestamp)) sourceFiles
    return $ Just sf
