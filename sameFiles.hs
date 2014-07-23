import Pipes.Prelude (foldM)  
import Data.List (group, sort)
import System.Environment (getArgs)
import Crypto.Hash.MD5 (hash)
import Control.Monad (when)
import Data.Map.Strict (Map, empty, alter)
import Data.Foldable (forM_)
import qualified Data.ByteString as B (ByteString, readFile)
import Util.StreamDirectory (getRecursiveContents)
import System.Posix (getFileStatus, fileSize, FileOffset)

possiblySimilarFiles ::  FilePath -> IO (Map FileOffset [FilePath])
possiblySimilarFiles dir = foldM add (return empty) return $ getRecursiveContents dir where
    add intmap file = do
        size <- fmap fileSize $ getFileStatus file
        let f Nothing = Just $! [file]
            f (Just list) = Just $! file:list
        return $! alter f size intmap

data HashPair = Pair {-# UNPACK #-} !B.ByteString !FilePath

instance Eq HashPair where
    (Pair h1 _) == (Pair h2 _) = h1 == h2

instance Ord HashPair where
    compare (Pair h1 _) (Pair h2 _) = compare h1 h2

instance Show HashPair where
    show (Pair _ filename) = filename

main :: IO ()
main = do
    [dir] <- getArgs
    collisionMap <- possiblySimilarFiles dir
    forM_ collisionMap $ \collision -> do
        when (sufficientlyLarge collision) $ do
            pairs <- mapM getHash collision
            mapM_ (\y -> when (sufficientlyLarge y) (print y)) . group . sort $ pairs where

            sufficientlyLarge (_:_:_) = True
            sufficientlyLarge _ = False

            getHash file = do 
                contents <- B.readFile file
                return $! Pair (hash contents) file
