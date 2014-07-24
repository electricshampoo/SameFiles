import Pipes.Prelude (foldM)
import Data.List (group, sort)
import System.Environment (getArgs)
import Crypto.Hash.MD5 (hash)
import Control.Monad (when)
import Control.Concurrent.Async (mapConcurrently)
import Data.Map.Strict (Map, empty, alter)
import qualified Data.HashMap.Strict as H (HashMap, insertWith, empty)
import Data.Foldable (forM_, foldrM)
import System.IO (withFile, IOMode(ReadMode))
import qualified Data.ByteString as B (ByteString, readFile, hGetSome)
import Util.StreamDirectory (getRecursiveContents)
import System.Posix (getFileStatus, fileSize, FileOffset)

sameSizeFiles :: FilePath -> IO (Map FileOffset [FilePath])
sameSizeFiles = foldM insert (return empty) return . getRecursiveContents where
    insert collisionMap file = do
        size <- fmap fileSize $ getFileStatus file
        return $! alter (Just . maybe [file] (file:)) size collisionMap

checkBytes :: Map a [FilePath] -> IO (H.HashMap B.ByteString [FilePath])
checkBytes = foldrM insert H.empty where
    insert collision collisionMap = if sufficientlyLarge collision then foldrM go collisionMap collision else return collisionMap
    go file cmap = do
        prefix <- withFile file ReadMode (flip B.hGetSome 64)
        return $! H.insertWith (\_ -> (file:)) prefix [file] cmap

data Pair = Pair {-# UNPACK #-} !B.ByteString !FilePath

instance Eq Pair where
    (Pair h1 _) == (Pair h2 _) = h1 == h2

instance Ord Pair where
    compare (Pair h1 _) (Pair h2 _) = compare h1 h2

instance Show Pair where
    show (Pair _ filename) = filename

sufficientlyLarge :: [a] -> Bool
sufficientlyLarge (_:_:_) = True
sufficientlyLarge _ = False

main :: IO ()
main = do
    [dir] <- getArgs
    collisionMap <- sameSizeFiles dir >>= checkBytes
    forM_ collisionMap $ \collision -> do
        when (sufficientlyLarge collision) $ do
            pairs <- mapConcurrently getHash collision
            mapConcurrently (\y -> when (sufficientlyLarge y) (print y)) . group . sort $ pairs
            return () where

            getHash file = do
                contents <- B.readFile file
                return $! Pair (hash contents) file
