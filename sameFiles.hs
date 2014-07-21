import Pipes (yield, liftIO, (~>))
import Pipes.Prelude (fold)
import Data.List (group, sort)
import System.Environment (getArgs)
import Crypto.Hash.SHA1 (hash)
import Control.Monad (when)
import System.IO (openFile, IOMode(ReadMode))
import qualified Data.ByteString as B (ByteString, readFile, hGetSome, readFile)
import qualified Data.HashMap.Strict as H (HashMap, insertWith, empty)
import qualified Data.Foldable as F (forM_)
import Util.StreamDirectory (getRecursiveContents)

possiblySimilarFiles :: Int -> FilePath -> IO (H.HashMap B.ByteString [FilePath])
possiblySimilarFiles n = fold (\hashmap (chunk, file) -> H.insertWith (++) chunk [file] hashmap)  H.empty id . getChunks where
    getChunks = getRecursiveContents ~> \file -> do
        chunck <- liftIO $ getNBytes n file
        yield (chunck, file)

getNBytes :: Int -> FilePath -> IO B.ByteString
getNBytes n file = do
    h <- openFile file ReadMode 
    B.hGetSome h n

data HashPair = Pair !B.ByteString !FilePath

instance Eq HashPair where
    (Pair h1 _) == (Pair h2 _) = h1 == h2

instance Ord HashPair where
    compare (Pair h1 _) (Pair h2 _) = compare h1 h2

instance Show HashPair where
    show (Pair _ filename) = filename

getHash :: FilePath -> IO HashPair
getHash file = do 
    contents <- B.readFile file
    return $! Pair (hash contents) file

main :: IO ()
main = do
    [dir, num] <- getArgs
    hashmap <- possiblySimilarFiles (read num) dir
    F.forM_ hashmap $ \x -> do
        when (sufficientlyLarge x) $ do
            pairs <- mapM getHash x
            mapM_ (\y -> when (sufficientlyLarge y) (print y)) . group . sort $ pairs
    where sufficientlyLarge [_] = False --lists are never empty so avoid checking for them
          sufficientlyLarge _ = True
