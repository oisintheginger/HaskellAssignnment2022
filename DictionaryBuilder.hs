import qualified Data.Set as Set
import qualified Data.Char as Chr
import Data.List as List
import Data.List.Split
import System.Environment
import System.IO  
replaceAllPunctuation :: Char -> Char
replaceAllPunctuation s = if (elem s ".,/:;?!()$%|&\n\b\0\r\t\v\&\\\"\'") then ' ' else s

filterPunctuation :: [Char] -> [Char]
filterPunctuation string = [replaceAllPunctuation s | s <- string]

preProcess :: [Char] -> [Char]
preProcess content = filterPunctuation content

processFile :: [Char] -> [Char]
processFile content = map (\l -> Chr.toLower l) (preProcess content)

main  = do
    [file1, file2, file3] <- getArgs
    firstFile <- readFile file1  
    secondFile <- readFile file2
    thirdFile <- readFile file3
    let f1 = lines firstFile 
        filteredFile1 = processFile (concat f1)
        listOfWords1 =  splitOn " " filteredFile1
    let f2 = lines secondFile
        filteredFile2 = processFile (concat f2)
        listOfWords2 =  splitOn " " filteredFile2
    let f3 = lines thirdFile
        filteredFile3 = processFile (concat f3)
        listOfWords3 =  splitOn " " filteredFile3
        combinedList = listOfWords3++listOfWords2++listOfWords1
        duplicatesRemoved = Set.toAscList (Set.fromList combinedList)
        outString = List.intercalate " " duplicatesRemoved
    writeFile ("Dictionary.txt") outString
