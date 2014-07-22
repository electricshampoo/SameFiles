import Pipes (liftIO, for)
import Focus (alterM)
import System.IO.Unsafe (unsafePerformIO)
import Pipes.Core (runEffect)
import Data.List (group, sort)
import Control.Concurrent (forkIO)
import System.Environment (getArgs)
import Crypto.Hash.SHA1 (hash)
import Control.Concurrent.Async (mapConcurrently, wait, withAsync)
import Control.Monad (when)
import System.IO (withFile, IOMode(ReadMode))
import Control.Concurrent.STM (atomically)
import STMContainers.Map (Map, focus, new, foldM)
import qualified Data.ByteString as B (ByteString, readFile, hGetSome)
import Util.StreamDirectory (getRecursiveContents)

possiblySimilarFiles :: Map B.ByteString [FilePath] -> Int -> FilePath -> IO ()
possiblySimilarFiles hashmap n dir = runEffect . for (getRecursiveContents dir) $ \file -> liftIO . (\x -> forkIO x >> return ()) $ do
        chunk <- withFile file ReadMode (flip B.hGetSome n)
        let f Nothing = return $! Just $! [file]
            f (Just list) = return $! Just $! file:list
        atomically $! focus (alterM f) chunk hashmap

data HashPair = Pair !B.ByteString !FilePath

instance Eq HashPair where
    (Pair h1 _) == (Pair h2 _) = h1 == h2

instance Ord HashPair where
    compare (Pair h1 _) (Pair h2 _) = compare h1 h2

instance Show HashPair where
    show (Pair _ filename) = filename

main :: IO ()
main = (\x -> withAsync x wait) $ do
    [dir, num] <- getArgs
    hashmap <- atomically new
    possiblySimilarFiles hashmap (read num) dir
    atomically $ foldM f () hashmap
    where 
        f _ (_,v) = when (sufficientlyLarge v) $ do
                        return $! unsafePerformIO $ do
                            pairs <- mapConcurrently getHash v
                            mapM_ (\y -> when (sufficientlyLarge y) (print y)) . group . sort $ pairs

        sufficientlyLarge [] = False
        sufficientlyLarge [_] = False 
        sufficientlyLarge _ = True

        getHash file = do 
            contents <- B.readFile file
            return $! Pair (hash contents) file
