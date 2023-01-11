import System.IO  
import Data.Char
import System.Environment
  
main = do  
   [filename,index] <- getArgs
   contents <- readFile filename  
   writeFile (filename++".chp") (encode (read index :: Int) contents)

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr(ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
 | isLower c = int2let ((let2int c + n) `mod` 26)
 | otherwise = c
 
encode :: Int -> String -> String
encode n s = [shift n (toLower c) | c <- s]