module Util.StreamDirectory
(getRecursiveContents
) where

import Control.Monad (forM_)
import Pipes (yield, lift, Producer)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

getRecursiveContents :: FilePath -> Producer FilePath IO ()
getRecursiveContents topPath = do
    names <- lift $ getDirectoryContents topPath
    let properNames = filter (\x -> x /= "." && x /= "..") names
    forM_ properNames $ \name -> do
        let path = topPath </> name
        isDirectory <- lift $ doesDirectoryExist path
        if isDirectory
            then getRecursiveContents path
            else yield path
