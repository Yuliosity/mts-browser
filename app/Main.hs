{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes)
import Data.String (fromString)
import qualified Data.Text.Lazy as T
import Prelude hiding (head, div, id)
import System.Directory
import System.Environment (getArgs)

import Text.Blaze.Html5 hiding (html, param, style, main, map)
import Text.Blaze.Html5.Attributes hiding (form, title, label)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty

data ProjectData = ProjectData
    { dataDesc :: !T.Text
    , dataLink :: !T.Text
    }

instance ToMarkup ProjectData where
    toMarkup (ProjectData desc link) = do
        "["
        a ! href (toValue link) $ toHtml desc
        "]"

data ProjectState
    = Done
    | InProgress
    | Error

instance ToMarkup ProjectState where
    toMarkup Done       = "Done"
    toMarkup InProgress = "In progress"
    toMarkup Error      = "Error"

type ProjectName = T.Text

data ProjectInfo = ProjectInfo
    { projectName :: !ProjectName
    , projectState :: !ProjectState
    , projectData :: [ProjectData]
    }

instance ToMarkup ProjectInfo where
    toMarkup (ProjectInfo name state data_) = tr $ do
        td $ toHtml name
        td $ toHtml state
        td $ mapM_ toHtml data_

filterDir :: [FilePath] -> [FilePath]
filterDir = filter (\dir -> dir /= "." && dir /= "..")

isLockedState :: FilePath -> IO Bool
isLockedState path = do
    let lockDir = path ++ "/.snakemake/locks"
    hasDir <- doesDirectoryExist lockDir
    if hasDir
        then (not . null . filterDir) `liftM` getDirectoryContents lockDir
        else return False

statsDir :: FilePath
statsDir = "/stats/summary/"

getProjectInfo :: FilePath -> ProjectName -> IO ProjectInfo
getProjectInfo dir name = do
    let path = dir ++ T.unpack name
    locked <- isLockedState path
    let state = if locked then InProgress else Done --TODO: detect errors
    let lookupStats :: T.Text -> FilePath -> IO (Maybe ProjectData)
        lookupStats title filename = do
            file <- doesFileExist (path ++ statsDir ++ filename)
            if file
                then return $ Just $ ProjectData title (name `T.append` T.pack ('/':filename))
                else return Nothing
    stats <- catMaybes `liftM` mapM (uncurry lookupStats)
        [ ("Post-propagation GF", "gf_bin_prop.tsv")
        , ("Reassembly GF", "gf_reassembly.tsv")
        , ("GF heatmap", "reassembly_gf.png")
        , ("GF heatmap (bad bins only)", "reassembly_bad.png")
        , ("Reassembly PCA plot", "pca_reassembly.png")
        , ("Bad assemblies report", "reassembly_problems.txt")
        ]
    return $ ProjectInfo name state stats

collectInfo :: FilePath -> [ProjectName] -> IO [ProjectInfo]
collectInfo basedir = mapM (getProjectInfo basedir)

main :: IO ()
main = do
    (dir:names) <- getArgs
    scotty 3000 $ do
        get "/" $ do
            projects <- liftIO $ collectInfo dir (map T.pack names)
            html $ renderHtml $ table $ mapM_ toHtml projects
        get "/:project/:file" $ do
            project <- param "project"
            filename <- param "file"
            file $ dir ++ project ++ statsDir ++ filename
