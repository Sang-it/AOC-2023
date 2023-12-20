module Three (partOne, partTwo) where

import Control.Arrow (second)
import Data.Char (isDigit, isSpace)
import Data.Foldable (foldl')
import qualified Data.Map as Map (elems, fromListWith)
import Data.Text (Text)
import qualified Data.Text as T (break, index, length, lines, pack, span)
import qualified Data.Text.Read as T (decimal)
import qualified Data.Vector as V (fromList, length, (!))

-- TODO: Write a diff solution
parts :: Text -> [((Int, Int), Int)]
parts input =
    [ ((x, y'), number)
    | line <- [0 .. V.length ls - 1]
    , ((start, end), number) <- numbers 0 $ ls V.! line
    , y' <- [max 0 $ line - 1 .. min (V.length ls - 1) $ line + 1]
    , let line = ls V.! y'
    , x <- [max 0 $ start - 1 .. min (T.length line - 1) end]
    , let c = T.index line x
    , not $ c == '.' || isSpace c || isDigit c
    ]
  where
    ls = V.fromList $ T.lines input

numbers acc line
    | (leading, rest) <- T.break isDigit line
    , Right (number, trailing) <- T.decimal rest =
        let acc' = acc + T.length line - T.length trailing
         in ((acc + T.length leading, acc'), number)
                : numbers acc' trailing
    | otherwise = []

partOne :: String -> Int
partOne = sum . map snd . parts . T.pack

partTwo :: String -> Int
partTwo = foldl' f 0 . Map.fromListWith (++) . map (second (: [])) . parts . T.pack
  where
    f k [x, y] = k + x * y
    f k _ = k
