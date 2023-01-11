import qualified Data.Set as Set
import qualified Data.Char as Chr
import System.Environment
import System.IO  

-- 1. is_square.
isWholeSqrRoot :: (Num a, Enum a, Eq a) => a -> Bool
isWholeSqrRoot n = length [ a | a <- [1..n], a*a == n] == 1


-- 2. freq_letter_pc
countFrequency:: [Char] -> Char -> (Char, Float)
countFrequency ls el= (el , sum [ 1 |e <- ls, e==el] * 100/ (sum [1 | _ <-ls])  )

letterCounter letter content = (letter, sum [1 | checkLetter <- content, checkLetter == letter] * 100 / (sum [ 1 |l <- content, l `elem` ['a'..'z']])) 

freq2 ls = [letterCounter letter ls | letter <- ['a'..'z']]

checkSet a = Set.toAscList (Set.fromList a)

remove_spaces s = [l| l<- s, l /= ' ']

-- THIS IS THE SOLUTION HERE 
mapFreq :: [Char] -> [(Char, Float)]
mapFreq ls = map (\el -> countFrequency ls el) (checkSet (remove_spaces ls))

-- 3. db in Haskell
cities :: [(Integer, [Char], Integer, Integer)]
cities = [(1,"Paris",7000000,2),(2,"London",8000000,1),(1,"Rome",3000000,3),(1,"Edinburgh",500000,1),(1,"Florence",50000,3),(1,"Venice",200000,3), (1,"Lyon",1000000,2),(1,"Milan",3000000,3),(1,"Madrid",6000000,4),(1,"Barcelona",5000000,4)]

countries :: [(Integer, [Char])]
countries = [(1,"UK"), (2,"France"), (3,"Italy"), (4,"Spain")]

getPop :: (a, b, c, d) -> c
getPop (_,_,pop,_) = pop

getName :: (a, b, c, d) -> b
getName (_,name,_,_) = name

getCityCountry :: (a, b, c, d) -> d
getCityCountry(_,_,_,countryId) = countryId

getCountryId :: [Char] -> Integer
getCountryId n =  [fst (l) | l <- countries, snd l == n] !! 0

findCountryByID :: Integer -> [Char]
findCountryByID id = [snd (l) | l <- countries, fst l == id] !! 0

-- a. get_city_above n – to get all the names of the cities with a population above n (=input)
get_city_above :: Integer -> [(Integer, [Char], Integer, Integer)]
get_city_above n = [c | c <- cities, getPop c > n ]

-- b. get_city country_name – to get all the cities given a country_name 
get_cities_in_country :: [Char] -> [[Char]]
get_cities_in_country n = [getName l | l <- cities, getCityCountry l ==  getCountryId n]

-- c. num_city– to list all the country and for each country the number of cities in each country.
getCityCount cntry = sum([1| cty <-  get_cities_in_country (snd cntry)])

num_cities :: [([Char], Integer)]
num_cities = [ (snd(cntry), getCityCount cntry )|cntry <-  countries]

-- 4. Euclidean distance between two lists
eucl_dist:: [Float] -> [Float] -> Float
eucl_dist xVector yVector = sqrt(sum (zipWith (\x y -> (x-y) * (x-y)) xVector yVector))

-- 5. Language Identification 
    -- The file lang.hs contains two lists of 26 float numbers each. The lists represent the frequency of
    -- each letter in English (list eng_freq) and Portuguese (pt_freq). For instance, the letter "b", theHas    
    -- second letter in the alphabet, has a frequency of 1.492% in English and 1.04% in Portuguese.
    -- Write a Haskell function get_lang that gets a text and print the message "The text is in English" if
    -- the text is written in English or "The text is in Portuguese" if the text is written in Portuguese (as a
    -- simplification, we only use text containing the 26 basic letters, without accents or other phonetic
    -- symbols). In order to identify the language, compute the frequency distribution of the letters in the
    -- text, store it into a list and check if the distribution is closer to eng_freq or pt_freq. You can use the
    -- function defined at exercise 2 and 6. Transform the text in small case first.
    -- Using the function readfile, create a version of the program running from command line that takes a
    -- file as an input (command line parameter) and detect the language of the file.

eng_freq :: [Float]
eng_freq = [8.12,1.49,2.71,4.32,12.02,2.30,2.03,5.92,7.31,0.10,0.69,3.98,2.61,6.95,7.68,1.82,0.11,6.02,6.28,9.10,2.88,1.11,2.09,0.17,2.11,0.07]

pt_freq :: [Float]
pt_freq = [12.21, 1.01, 3.35, 4.21, 13.19, 1.07, 1.08, 1.22, 5.49, 0.30, 0.13, 3.00, 5.07, 5.02, 10.22, 3.01, 1.10, 6.73, 7.35, 5.07, 4.46, 1.72, 0.05, 0.28, 0.04, 0.45]

check_all_letters_freq :: [Char] -> [(Char, Float)]
check_all_letters_freq s = [(countFrequency s l)| l<- ['a'..'z']]  

eng_or_pt :: [Char] -> (String, [Char])
eng_or_pt s = if (eucl_dist [b| (a,b) <- check_all_letters_freq s] eng_freq) < (eucl_dist [b| (a,b) <- check_all_letters_freq s] pt_freq) then ("This is english", s) else ("this is portuguese", s)

check_lang :: [Char] -> (String, [Char])
check_lang s = eng_or_pt (map (\l -> Chr.toLower l) s)

main  = do
    [filename] <- getArgs
    contents <- readFile filename  
    let c = lines contents 
        res = check_lang (concat c)
        answer = fst res
        in_text = snd res
    putStrLn in_text
    putStrLn answer