module Main where

import qualified One (partOne, partTwo)
import System.FilePath (takeBaseName)
import qualified Three (partOne, partTwo)
import qualified Two (partOne, partTwo)

run :: (String -> Int) -> (String -> Int) -> FilePath -> IO ()
run partOne partTwo path = do
    input <- readFile path
    print $ takeBaseName path ++ " | Part One: " ++ show (partOne input) ++ " | Part Two: " ++ show (partTwo input)

main :: IO ()
main = do
    run One.partOne One.partTwo "input/One.input"
    run Two.partOne Two.partTwo "input/Two.input"
    run Three.partOne Three.partTwo "input/Three.input"
