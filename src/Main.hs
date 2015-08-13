
import Control.Applicative
import Data.Monoid
import Data.Traversable (traverse)
import System.Environment
import qualified Data.Vector as V

main :: IO ()
main = do
    levelName <- head <$> getArgs
    Just level <- parseLevel <$> readFile levelName
    print level

data Dir = U | D | L | R

data Tile
    = Empty
    | Wall
    | Fruit
    | Exit
    | Head
    | Tail Dir

instance Show Tile where
    show Empty = "."
    show Wall = "#"
    show Fruit = "F"
    show Exit = "E"
    show Head = "B"
    show (Tail U) = "^"
    show (Tail D) = "v"
    show (Tail L) = "<"
    show (Tail R) = ">"

newtype Level = Level
    { _levelGrid :: V.Vector (V.Vector Tile)
    }

instance Show Level where
    show (Level grid) =
        let showLine tiles = concat (V.toList (V.map show tiles))
        in unlines (V.toList (V.map showLine grid))

parseLevel :: String -> Maybe Level
parseLevel s = do
    level <- Level <$> traverse parseLine (V.fromList (lines s))
    Just (padEmpty level)


padEmpty :: Level -> Level
padEmpty (Level rows) =
    Level (V.map padRow rows)
    where
    padRow row = row <> V.replicate (maxLen - V.length row) Empty
    maxLen = V.maximum (V.map V.length rows)

parseLine :: String -> Maybe (V.Vector Tile)
parseLine = traverse parseTile . V.fromList
    where parseTile ' ' = Just Empty
          parseTile '#' = Just Wall
          parseTile 'F' = Just Fruit
          parseTile 'E' = Just Exit
          parseTile 'B' = Just Head
          parseTile '^' = Just (Tail U)
          parseTile 'v' = Just (Tail D)
          parseTile '<' = Just (Tail L)
          parseTile '>' = Just (Tail R)
          parseTile x = error ("unrecognized tile " <> show x)

applyGravity :: Level -> Maybe Level
applyGravity l = Just l
