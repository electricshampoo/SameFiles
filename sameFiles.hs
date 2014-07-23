import Pipes.Prelude (foldM)  
import Data.List (group, sort)
import System.Environment (getArgs)
import Crypto.Hash.MD5 (hash)
import Control.Monad (when)
import Data.Map.Strict (Map, empty, alter)
import Data.Foldable (forM_, foldr', foldrM)
import System.IO (withFile, IOMode(ReadMode))
import qualified Data.ByteString as B (ByteString, readFile, hGetSome)
import Util.StreamDirectory (getRecursiveContents)
import System.Posix (getFileStatus, fileSize, FileOffset)

sameSizeFiles ::  FilePath -> IO (Map FileOffset [FilePath])
sameSizeFiles = foldM add (return empty) return . getRecursiveContents where
    add collisionMap file = do
        size <- fmap fileSize $ getFileStatus file
        return $! alter (Just . maybe [file] (file:)) size collisionMap

checkBytes :: Map a [FilePath] -> IO (Map B.ByteString [FilePath])
checkBytes = foldrM add empty where
    add collision collisionMap = if sufficientlyLarge collision then do
        pairs <- flip mapM collision $ \file -> do
            prefix <- withFile file ReadMode (flip B.hGetSome 64)
            return $! Pair prefix file
        return $! foldr' (\(Pair prefix file) cmap -> alter (Just . maybe [file] (file:)) prefix cmap) collisionMap pairs
        else return collisionMap
        
data Pair = Pair {-# UNPACK #-} !B.ByteString !FilePath

instance Eq Pair where
    (Pair h1 _) == (Pair h2 _) = h1 == h2

instance Ord Pair where
    compare (Pair h1 _) (Pair h2 _) = compare h1 h2

instance Show Pair where
    show (Pair _ filename) = filename

main :: IO ()
main = do
    [dir] <- getArgs
    collisionMap <- sameSizeFiles dir >>= checkBytes
    forM_ collisionMap $ \collision -> do
        when (sufficientlyLarge collision) $ do
            pairs <- mapM getHash collision
            mapM_ (\y -> when (sufficientlyLarge y) (print y)) . group . sort $ pairs where

            getHash file = do 
                contents <- B.readFile file
                return $! Pair (hash contents) file

sufficientlyLarge :: [a] -> Bool
sufficientlyLarge (_:_:_) = True
sufficientlyLarge _ = False

