{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Reader (ask)

import Data.List (sortBy, (\\))
import Data.Maybe (listToMaybe)
import Data.String.Utils (replace)
import qualified Data.ConfigFile as ConfigFile
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import Data.Either.Utils (forceEither)
import Data.Digest.Pure.SHA (sha256, showDigest)
import System.Directory (getHomeDirectory, createDirectoryIfMissing, getDirectoryContents)
import qualified Data.Text as T

import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable (Typeable)
import Data.DateTime (getCurrentTime, DateTime)

import           Control.Applicative

import Snap
import Snap.Snaplet.Heist
import Snap.Snaplet.AcidState
import Snap.Util.FileServe (serveDirectory)
import Snap.Util.FileUploads (handleMultipart, defaultUploadPolicy)
import Snap.Iteratee (consume)
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
                ("giveFile", giveFile),
                ("uploadFile/:hash", uploadFile),
                ("static", serveDirectory source)
            ]

    return app

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


-- ROUTES
workList :: Handler App App ()
workList = do
    s <- query ReadState
    let sf = s ^. stateSourceFiles
    let source = s ^. stateSourceDir

    renderWithSplices "list" $ allWorkItems sf


giveFile :: Handler App App ()
giveFile = do
    now <- liftIO $ getCurrentTime

    s <- query ReadState
    let sfs = s  ^. stateSourceFiles
    let gfs = s ^. stateFilesGiven

    let source = s ^. stateSourceDir

    liftIO $ print $ show gfs

    let gfsHashes = map (\l -> l ^. sourceFileHash) gfs

    let rem = filter (\l -> (l ^. sourceFileHash) `notElem` gfsHashes) sfs
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

    renderWithSplices "giveFile" $ giveFileItem sf source


uploadFile :: Handler App App ()
uploadFile = method GET getter <|> method POST setter
    where
        getter = do
            esf <- hashReader
            case esf of
                Left err -> writeBS err
                Right sf -> do
                    --writeBS $ B.pack $ show sf

                    s <- query ReadState
                    let source = s ^. stateSourceDir
                    renderWithSplices "uploadFile" $ giveFileItem sf source

        setter = do
            esf <- hashReader
            liftIO $ print $ show $ esf
            case esf of
                Left err -> writeBS err
                Right sf -> do
                    [file] <- handleMultipart defaultUploadPolicy $ \part -> do
                        content <- liftM B.concat consume
                        return content

                    s <- query ReadState
                    let source = s ^. stateSourceDir
                    let target = s ^. stateTargetDir
                    let path = replace source target $ sf ^. sourceFilePath

                    liftIO $ BS.writeFile path file

                    let sfs = filter
                                (\sf' -> (sf' ^. sourceFileHash) /= (sf ^. sourceFileHash)) $
                                s ^. stateSourceFiles
                    let s' = stateSourceFiles .~ sfs $ s
                    update $ WriteState s'

                    writeBS $ B.pack $ show sf


        hashReader = do
            mhash <- getParam "hash"
            case mhash of
                Nothing -> return $ Left "No hash provided"
                Just hash' -> do
                    let hash = B.unpack hash'
                    s <- query ReadState
                    let msf = sourceFileFromAppStateByHash s hash
                    case msf of
                        Nothing -> return $ Left "File not found"
                        Just sf -> return $ Right sf


sourceFileFromAppStateByHash :: AppState -> Hash -> Maybe SourceFile
sourceFileFromAppStateByHash s hash = listToMaybe filtered
    where
        sfs = s  ^. stateSourceFiles
        filtered = filter (\l -> (l ^. sourceFileHash) == hash) sfs


-- RENDERERS
allWorkItems :: [SourceFile] -> Splices (SnapletISplice App)
allWorkItems sfs = "items" ## (mapSplices $ runChildrenWith . listItem) sfs

listItem :: Monad m => SourceFile -> Splices (Splice m)
listItem sf = do
    let hi = hashItem sf

    "listItem" ## hi
    "listItemURL" ## hi

giveFileItem :: SourceFile -> SourceDir -> Splices (SnapletISplice App)
giveFileItem sf source = "file" ## runChildrenWith $ do
    let pi = pathItem sf source
    let hi = hashItem sf

    "path" ## pi
    "hash" ## hi

pathItem :: Monad m => SourceFile -> SourceDir -> Splice m
pathItem sf source = do
    let fp = replace source "/static" $ sf ^. sourceFilePath

    textSplice (T.pack fp)

hashItem :: Monad m => SourceFile -> Splice m
hashItem sf = textSplice $ T.pack $ sf ^. sourceFileHash

