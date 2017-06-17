import System.Environment
import qualified Data.ByteString.Char8
--import qualified Data.ByteStringLazy as L
--import qualified Data.ByteString as S

main = do
    (source:dest:filterMatch:_) <- getArgs
    filterLines source dest filterMatch
    
filterLines :: FilePath -> FilePath -> String -> IO ()
filterLines source dest fMatch = do
    contents <- C.readFile source
    let lines' = C.lines contents
        filterOut = C.pack fMatch
        contents' = C.unlines . filter (C.isInfixOf filterOut) $ lines'
    C.writeFile dest contents'
    
    
    
-- filterLines "1.csv" "1f.csv" ",BB10"
