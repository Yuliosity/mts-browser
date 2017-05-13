{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.String (fromString)
import Prelude hiding (head, div, id)
import System.Directory
import System.Environment (getArgs)
import Web.Scotty
import qualified Data.Text.Lazy as T
import Text.Blaze.Html5 hiding (html, param, style, main, map)
import Text.Blaze.Html5.Attributes hiding (form, title, label)
import Text.Blaze.Html.Renderer.Text (renderHtml)

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
    hasDir <- doesDirectoryExist (path ++ "/.snakemake/locks")
    if hasDir
        then (not . null . filterDir) `liftM` getDirectoryContents (path ++ "/.snakemake/locks")
        else return False

getProjectInfo :: FilePath -> ProjectName -> IO ProjectInfo
getProjectInfo dir name = do
    let path = dir ++ T.unpack name
    locked <- isLockedState path
    let state = if locked then InProgress else Done --TODO: detect errors
    reassemblyTableExists <- doesFileExist (path ++ "/stats/summary/gf_reassembly.tsv")
    --TODO: other stats
    let stats = if reassemblyTableExists
        then [ProjectData "Reassembly GF" (name `T.append` "/gf_reassembly.tsv")]
        else []
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
            file $ dir ++ project ++ "/stats/summary/" ++ filename
