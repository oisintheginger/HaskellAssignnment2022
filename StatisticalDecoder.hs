import System.IO  
import Data.Char
import System.Environment
import Data.List as List
import Data.List.Split

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr(ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
 | isLower c = int2let ((let2int c - n) `mod` 26)
 | otherwise = c
 
decode :: Int -> String -> String
decode n s = [shift n (toLower c) | c <- s]

multipleDecrypt :: String -> [[[Char]]]
multipleDecrypt c = [(splitOn " " (decode n c)) | n <- [0..26]]

countMatchingWords :: (Num b, Foldable t, Eq a) => [a] -> t a -> ([a], b)
countMatchingWords contender dictionary = ( contender, sum [1 | s <- contender, elem s dictionary])


processDecryptions :: (Num b, Foldable t, Eq a) => [[a]] -> t a -> [([a], b)]
processDecryptions ls dictionary = [(countMatchingWords contender dictionary)| contender <- ls]


prepareForWrite :: ([[Char]], b) -> [Char]
prepareForWrite res = List.intercalate " " (fst res)


findMax :: Ord a => [([[Char]], a)] -> [Char]
findMax (d:ds) = tupleMaximum d ds
    where   tupleMaximum maxVal [] = prepareForWrite maxVal
            tupleMaximum (words, maxMatch) (decryption: decryptionsList)
                | maxMatch < (snd decryption) = tupleMaximum decryption decryptionsList
                | otherwise = tupleMaximum (words, maxMatch) decryptionsList

main = do 
    [fileToDecrypt, dictionaryFile] <- getArgs 
    contents <- readFile fileToDecrypt
    dictionaryContent <- readFile dictionaryFile
    let c = lines contents
        decryptionResults = multipleDecrypt (concat c)
    let d = lines dictionaryContent
        dictionary =  splitOn " " (concat d)
        res = processDecryptions decryptionResults dictionary
        decryptionResult = findMax res
    print decryptionResult
    writeFile ("DecryptionResult.txt") decryptionResult
