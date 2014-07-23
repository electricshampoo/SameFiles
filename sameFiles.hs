import Pipes.Prelude (foldM)  
import Data.List (group, sort)
import System.Environment (getArgs)
import Crypto.Hash.SHA1 (hash)
import Control.Monad (when)
import Data.Map.Strict (Map, empty, alter)
import Data.Foldable (forM_)
import qualified Data.ByteString as B (ByteString, readFile)
import Util.StreamDirectory (getRecursiveContents)
import System.Posix (getFileStatus, fileSize, FileOffset)

possiblySimilarFiles ::  FilePath -> IO (Map FileOffset [FilePath])
possiblySimilarFiles dir = foldM add (return empty) return (getRecursiveContents dir) where
    add intmap file = do
        size <- fmap fileSize $ getFileStatus file
        let f :: Maybe [FilePath] -> Maybe [FilePath]
            f Nothing = Just $! [file]
            f (Just list) = Just $! file:list
        return $! alter f size intmap

data HashPair = Pair !B.ByteString !FilePath

instance Eq HashPair where
    (Pair h1 _) == (Pair h2 _) = h1 == h2

instance Ord HashPair where
    compare (Pair h1 _) (Pair h2 _) = compare h1 h2

instance Show HashPair where
    show (Pair _ filename) = filename

main :: IO ()
main = do
    [dir] <- getArgs
    intmap <- possiblySimilarFiles dir
    forM_ intmap $ \collisions -> do
        when (sufficientlyLarge collisions) $ do
            pairs <- mapM getHash collisions
            mapM_ (\y -> when (sufficientlyLarge y) (print y)) . group . sort $ pairs where

        sufficientlyLarge [] = False
        sufficientlyLarge [_] = False 
        sufficientlyLarge _ = True

        getHash file = do 
            contents <- B.readFile file
            return $! Pair (hash contents) file
