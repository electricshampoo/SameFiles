import Pipes (yield, lift, (~>))
import Pipes.Prelude (fold)
import System.Environment (getArgs)
import Crypto.Hash.SHA1 (hash)
import qualified Data.ByteString as B (ByteString, readFile)
import qualified Data.HashMap.Strict as H (HashMap, insertWith, empty)
import qualified Data.Foldable as F (mapM_)
import Util.StreamDirectory (getRecursiveContents)

similarFiles :: FilePath -> IO (H.HashMap B.ByteString [FilePath])
similarFiles = fold (\hashmap (sha1,file) -> H.insertWith (++) sha1 [file] hashmap)  H.empty id . getHashes where
    getHashes = getRecursiveContents ~> \file -> do
        contents <- lift $ B.readFile file
        yield (hash contents, file)

main :: IO ()
main = do
    [dir] <- getArgs
    hashmap <- similarFiles dir
    F.mapM_ (\x -> if length x > 1 then print x else return ()) hashmap
