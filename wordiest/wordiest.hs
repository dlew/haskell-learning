import qualified Data.Map.Strict as Map  

import System.IO
import Data.Char
import Data.List
import Data.Maybe

-- Could do input better, don't care
input = 
    [ tile 's'
    , tile 'd'
    , tile 'm'
    , tileSingle 'n' 2
    , tile 'z'
    , tileWord 'e' 2
    , tile 'g'
    , tile 's'
    , tile 'i'
    , tile 'e'
    , tileWord 'y' 2
    , tile 'r'
    , tileSingle 'a' 3
    , tile 'a' ]

main = do
    contents <- readFile "sowpods.txt"
    let dictionary = loadDictionary contents
        best = findSingleBestWord (tail $ subsequences input) dictionary
    putStrLn (show best)

-- Solver

findSingleBestWord :: [TileSet] -> Dictionary -> (Word, Int)
findSingleBestWord sets dictionary = foldl' folder ("", 0) sets
    where folder = (\acc set -> getBetterValue acc (getTileSetValueInDictionary set dictionary))

getBetterValue :: (Word, Int) -> (Word, Int) -> (Word, Int)
getBetterValue (a, x) (b, y) =
    if x > y
        then (a, x)
        else (b, y)

getTileSetValueInDictionary :: TileSet -> Dictionary -> (Word, Int)
getTileSetValueInDictionary set dictionary =
    if isWordInDictionary word dictionary
        then (getWordFromDictionary word dictionary, getTileSetValue set)
        else ("", 0)
    where word = getTileSetWord set

-- Dictionary

loadDictionary :: String -> Dictionary
loadDictionary contents =
    let lower = map toLower contents
        split = [(sort word, word) | word <- lines lower]
    in  Map.fromList split

isWordInDictionary :: Word -> Dictionary -> Bool
isWordInDictionary word = Map.member (sort word)

getWordFromDictionary :: Word -> Dictionary -> Word
getWordFromDictionary word dict = fromJust (Map.lookup (sort word) dict)

-- Our types! --

data Alphabet = Alphabet Char deriving (Show)
data Multiplier = Single Int | Word Int deriving (Show)
data Tile = Tile Multiplier Alphabet deriving (Show)
type TileSet = [Tile]
type Word = String
type SortedWord = Word
type Dictionary = Map.Map SortedWord Word

toAlphabet :: Char -> Alphabet
toAlphabet c
    | c `elem` "abcdefghijklmnopqrstuvwxyz" = Alphabet c
    | otherwise = error "invalid char"

letterValue :: Alphabet -> Int
letterValue (Alphabet 'a') = 1
letterValue (Alphabet 'b') = 4
letterValue (Alphabet 'c') = 3
letterValue (Alphabet 'd') = 2
letterValue (Alphabet 'e') = 1
letterValue (Alphabet 'f') = 4
letterValue (Alphabet 'g') = 3
letterValue (Alphabet 'h') = 3
letterValue (Alphabet 'i') = 1
letterValue (Alphabet 'j') = 10
letterValue (Alphabet 'k') = 5
letterValue (Alphabet 'l') = 2
letterValue (Alphabet 'm') = 4
letterValue (Alphabet 'n') = 2
letterValue (Alphabet 'o') = 1
letterValue (Alphabet 'p') = 3
letterValue (Alphabet 'q') = 10
letterValue (Alphabet 'r') = 1
letterValue (Alphabet 's') = 1
letterValue (Alphabet 't') = 1
letterValue (Alphabet 'u') = 2
letterValue (Alphabet 'v') = 6
letterValue (Alphabet 'w') = 4
letterValue (Alphabet 'x') = 8
letterValue (Alphabet 'y') = 4
letterValue (Alphabet 'z') = 10
letterValue (Alphabet c) = error $ "Unexpected Alphabet character '" ++ [c] ++ "'"

-- Easy constructors for tiles
tile :: Char -> Tile
tile c = Tile (Single 1) (Alphabet c)

tileSingle :: Char -> Int -> Tile
tileSingle c x = Tile (Single x) (Alphabet c)

tileWord :: Char -> Int -> Tile
tileWord c x = Tile (Word x) (Alphabet c)

-- Value calculations --

-- Return value of individual letter (taking into account individual, but not word, multiplication)
getTileValue :: Tile -> Int
getTileValue (Tile (Single n) c) = (*n) $ letterValue c
getTileValue (Tile _ c) = letterValue c

getTileChar :: Tile -> Char
getTileChar (Tile _ (Alphabet c)) = c

getWordMultiplier :: Multiplier -> Int
getWordMultiplier (Word n) = n
getWordMultiplier _ = 1

-- Return word multiplier portion of a letter
getTileWordMultiplier :: Tile -> Int
getTileWordMultiplier (Tile multiplier _) = getWordMultiplier multiplier

-- Gets overall word multiplier value for a set of letters
getTileSetMultiplier :: TileSet -> Int
getTileSetMultiplier = foldl (\acc letter -> acc * (getTileWordMultiplier letter)) 1

getTileSetValue :: TileSet -> Int
getTileSetValue set = 
    let baseValue = foldl (\acc x -> acc + getTileValue x) 0 set
        multiplier = getTileSetMultiplier set
	in baseValue * multiplier

getTileSetWord :: TileSet -> SortedWord
getTileSetWord set = 
    let word = foldl (\acc tile -> acc ++ [getTileChar tile]) "" set
    in sort word
