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
-- TODO: Doesn't work if the words overlap
-- Example : "oneight" is read as 1 instead of 8
--
-- Failed Attempt
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

parseLine :: String -> [String]
parseLine line = fromRight ["failed"] (P.parse (P.many parseNumberLike) "" line)

parseInput :: String -> [[String]]
parseInput input = map parseLine $ lines input

combine [] = []
combine [x] = x
combine (x : xs) = x ++ combine xs

sumTwo input = map (removeNoise . combine) $ parseInput input

--- Failed Attempt

parseDigitSpelled :: String -> Maybe Int
parseDigitSpelled [] = Nothing
parseDigitSpelled input@(x : _)
    | "one" `isPrefixOf` input = Just 1
    | "two" `isPrefixOf` input = Just 2
    | "three" `isPrefixOf` input = Just 3
    | "four" `isPrefixOf` input = Just 4
    | "five" `isPrefixOf` input = Just 5
    | "six" `isPrefixOf` input = Just 6
    | "seven" `isPrefixOf` input = Just 7
    | "eight" `isPrefixOf` input = Just 8
    | "nine" `isPrefixOf` input = Just 9
    | isDigit x = Just $ digitToInt x
    | otherwise = Nothing

firstAndLastDigitsNum :: (String -> Maybe Int) -> String -> Int
firstAndLastDigitsNum maybeDigit input = head digits * 10 + last digits
  where
    digits = mapMaybe maybeDigit $ tails input

findSumTwo :: String -> Int
findSumTwo input = sum $ map (firstAndLastDigitsNum parseDigitSpelled) $ lines input

main :: IO ()
main = do
    input <- readFile "input/One.input"
    print $ findSumOne input
    print $ findSumTwo input
