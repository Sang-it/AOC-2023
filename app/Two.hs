module Two where

import Data.Either (fromRight)
import Data.List (maximumBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import qualified Text.ParserCombinators.Parsec as P

-- Part One
numberOfBalls :: [(String, Integer)]
numberOfBalls =
    [ ("red", 12)
    , ("green", 13)
    , ("blue", 14)
    ]

parsePrefix :: P.Parser Integer
parsePrefix = do
    P.string "Game"
    P.try P.spaces
    gameId <- P.many P.digit
    P.string ":"
    P.try P.spaces
    return $ read gameId

parseLine :: P.Parser Integer
parseLine = do
    gameId <- parsePrefix
    colors <- P.sepBy (P.sepBy parseColor $ P.char ',') (P.char ';')
    if all and colors
        then return gameId
        else return 0

parseColor :: P.Parser Bool
parseColor = do
    P.try P.spaces
    n <- P.many P.digit
    P.try P.spaces
    c <- P.choice [P.string "red", P.string "blue", P.string "green"]
    let t = fromJust $ lookup c numberOfBalls
    return (t >= read n)

findSumOne input = sum $ map (fromRight 0 . P.parse parseLine "") $ lines input

-- Part Two
parseLine_ = do
    parsePrefix
    colors <- P.sepBy (P.sepBy parseColor_ $ P.char ',') (P.char ';')
    let red = fromJust $ lookUpMax "red" $ concat colors
    let blue = fromJust $ lookUpMax "blue" $ concat colors
    let green = fromJust $ lookUpMax "green" $ concat colors
    return $ product $ map snd [red, blue, green]

parseColor_ :: P.Parser (String, Integer)
parseColor_ = do
    P.try P.spaces
    n <- P.many P.digit
    P.try P.spaces
    c <- P.choice [P.string "red", P.string "blue", P.string "green"]
    return (c, read n)

lookUpMax :: String -> [(String, Integer)] -> Maybe (String, Integer)
lookUpMax val xs = case filter (\(key, _) -> key == val) xs of
    [] -> Nothing
    values -> Just $ maximumBy (comparing snd) values

findSumTwo input = sum $ map (fromRight 0 . P.parse parseLine_ "") $ lines input

main :: IO ()
main = do
    input <- readFile "input/Two.input"
    print $ findSumOne input
    print $ findSumTwo input
