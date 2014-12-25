import qualified Data.Map.Strict as Map  

import System.IO
import Data.Char
import Data.List
import Data.Maybe

-- Could do input better, don't care
input = 
    [ tile 's' 1
    , tile 'd' 1
    , tile 'm' 1
    , tile 'n' 2
    , tile 'z' 1
    , tileWord 'e' 2
    , tile 'g' 1
    , tile 's' 1
    , tile 'i' 1
    , tile 'e' 1
    , tileWord 'y' 2
    , tile 'r' 1
    , tile 'a' 3
    , tile 'a' 1 ]

main = do
    contents <- readFile "sowpods.txt"
    let dictionary = loadDictionary contents
        validTileSets = findValidTileSets (tail $ subsequences input) dictionary
        bestTileSet = findBestTileSet validTileSets
        bestWord = getWordFromDictionary (getTileSetWord bestTileSet) dictionary
        bestValue = getTileSetValue bestTileSet
    putStrLn (show bestWord ++ " is worth " ++ (show bestValue) ++ " points")

-- Solver (for single best word)

-- Find all sets that have valid words in the dictionary
findValidTileSets :: [TileSet] -> Dictionary -> [TileSet]
findValidTileSets sets dict = filter f sets
    where f = (\set -> isWordInDictionary (sort (getTileSetWord set)) dict)

findBestTileSet :: [TileSet] -> TileSet
findBestTileSet sets = foldl' (\set acc -> compareTileSets set acc) [] sets

compareTileSets :: TileSet -> TileSet -> TileSet
compareTileSets set1 set2 =
    if getTileSetValue set1 > getTileSetValue set2
        then set1
        else set2

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
tile :: Char -> Int -> Tile
tile c x = Tile (Single x) (Alphabet c)

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
getTileSetMultiplier = foldl' (\acc letter -> acc * (getTileWordMultiplier letter)) 1

getTileSetValue :: TileSet -> Int
getTileSetValue set = 
    let baseValue = foldl' (\acc x -> acc + getTileValue x) 0 set
        multiplier = getTileSetMultiplier set
	in baseValue * multiplier

getTileSetWord :: TileSet -> SortedWord
getTileSetWord set = 
    let word = foldl' (\acc tile -> acc ++ [getTileChar tile]) "" set
    in sort word
