import Pipes (yield, lift, (~>))
import Pipes.Prelude (fold)
import System.Environment (getArgs)
import Crypto.Hash.SHA1 (hash)
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as H
import qualified Data.Traversable as T
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
    _ <- T.mapM (\x -> if length x > 1 then print x else return ()) hashmap
    return ()
