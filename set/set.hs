import Data.List

type Attribute = Char
type Card = [Attribute]
type Set = [Card]

findSets :: Int -> [Card] -> [Set]
findSets 0 _ = []
findSets _ [] = []
findSets n cards = 
    if valid 
        then sets
        else error "Invalid input, all cards must have same number of attributes"
    where valid = allCardsEqualLength cards
          possibilities = combinations n cards
          sets = foldl' (\acc set -> if isValidSet set then acc++[set] else acc) [] possibilities

combinations :: Int -> [a] -> [[a]]
combinations 0 xs = [[]]
combinations _ [] = []
combinations 1 xs = [[x] | x <- xs]
combinations n (x:xs) = [x:y | y <- combinations (n-1) xs] ++ combinations n xs

-- Length of all Cards must be equal
-- EACH attribute must be identical OR unique across all cards
isValidSet :: Set -> Bool
isValidSet set = and [isValidAttribute x | x <- transpose set]

isValidAttribute :: [Attribute] -> Bool
isValidAttribute attributes = allSame || allDifferent
    where nubs = nub attributes
          numNubs = length nubs
          allSame = numNubs == 1
          allDifferent = numNubs == length attributes

-- Error checking
allCardsEqualLength :: Set -> Bool
allCardsEqualLength set = length nubs == 1
    where equalLength = (\card1 card2 -> length card1 == length card2)
          nubs = nubBy equalLength set