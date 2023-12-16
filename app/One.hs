import Control.Applicative ((<|>))
import Control.Arrow (right)
import Data.Char (digitToInt, isDigit, isNumber)
import Data.Either (fromRight, rights)
import Data.List (elemIndex, intersperse, isPrefixOf, tails)
import Data.Maybe (fromJust, mapMaybe)
import qualified Text.Parsec.Char as PS
import qualified Text.ParserCombinators.Parsec as P

-- Part One
parseNumber :: String -> String
parseNumber = filter isNumber

removeNoise :: String -> Int
removeNoise xs = read (head xs : [last xs])

findSumOne :: String -> Int
findSumOne input = sum $ map (removeNoise . parseNumber) (lines input)

-- Part Two
spelledNums =
    [ ("one", 1)
    , ("two", 2)
    , ("three", 3)
    , ("four", 4)
    , ("five", 5)
    , ("six", 6)
    , ("seven", 7)
    , ("eight", 8)
    , ("nine", 9)
    ]

parseDigit :: P.Parser String
parseDigit = P.many1 P.digit

parseStrings :: P.Parser String
parseStrings =
    do
        x <-
            P.choice
                [ PS.string' "one"
                , PS.string' "two"
                , PS.string' "three"
                , PS.string' "four"
                , PS.string' "five"
                , PS.string' "six"
                , PS.string' "seven"
                , PS.string' "eight"
                , PS.string' "nine"
                ]
        return $ show (fromJust (lookup x spelledNums))

parseNoise :: P.Parser String
parseNoise = skipTill P.letter (parseStrings <|> parseDigit)

skipTill p end = scan
  where
    scan = end <|> do _ <- p; scan

parseNumberLike :: P.Parser String
parseNumberLike =
    parseDigit
        <|> parseStrings
        <|> P.try parseNoise

parseSubstring :: String -> [String]
parseSubstring substring = fromRight ["failed"] (P.parse (P.many parseNumberLike) "" substring)

getDigit :: String -> [[String]]
getDigit input = map parseSubstring $ tails input

parseLine :: String -> Int
parseLine line = removeNoise $ combine $ map combine $ getDigit line

combine [] = []
combine [x] = x
combine (x : xs) = x ++ combine xs

findSumTwo :: String -> Int
findSumTwo input = sum $ map parseLine $ lines input

main :: IO ()
main = do
    input <- readFile "input/One.input"
    print $ findSumTwo input
