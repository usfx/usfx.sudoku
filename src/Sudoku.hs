module Sudoku where
//Author: Jim
//Modifier: Youssef
import Data.Char (digitToInt)
import Data.Maybe (fromJust, isJust, isNothing, listToMaybe)
import Data.List (transpose, group, sort, elemIndex)
import Data.List.Split (chunksOf)
import Control.Monad (liftM, replicateM_)


import Data.Ix (inRange)

-------------------------------------------------------------------------

{-| A Sudoku puzzle is a list of lists, where each value is a Maybe Int. That is,
each value is either `Nothing' or `Just n', for some Int value `n'. |-}
data Puzzle = Puzzle [[Maybe Int]]
 deriving (Show, Eq)

{-| A Block is a list of 9 Maybe Int values. Each Block represents a row, a column,
or a square. |-}
type Block = [Maybe Int]

{-| A Pos is a zero-based (row, column) position within the puzzle. |-}
data Pos = Pos (Int, Int) deriving (Show, Eq)


{-| A getter for the rows in a Sudoku puzzle. |-}
rows :: Puzzle -> [[Maybe Int]]
rows (Puzzle rs) = rs

example :: Puzzle
example =
  Puzzle
    [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
    , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
    , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
    , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
    , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
    , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
    , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
    , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
    , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
    ]

{-| Ex 1.1

    A sudoku with just blanks. |-}
allBlankPuzzle :: Puzzle
allBlankPuzzle =  Puzzle (replicate 9 (replicate 9 Nothing))

{-| Ex 1.2

    Checks if sud is really a valid representation of a sudoku puzzle. |-}
isPuzzle :: Puzzle -> Bool
isPuzzle puzzle = let rs = rows puzzle
                  in
                   length rs == 9
                   && all ((== 9) . length) rs
                   && and (concatMap
                             (map (\ x -> 
                                    maybe (isNothing x) (inRange (1, 9)) x))
                             rs)

{-| Ex 1.3

    Checks if the puzzle is already solved, i.e. there are no blanks. |-}
isSolved :: Puzzle -> Bool
isSolved =  and . concatMap (map isJust) . rows

{-| Ex 2.1

    `printPuzzle s' prints a representation of `s'. |-}
printPuzzle :: Puzzle -> IO ()
printPuzzle = putStrLn . unlines . map printaRow . rows

--Convert a row into a String

printaRow :: Show a => [Maybe a] -> String
printaRow [] = "";
printaRow (x:xs) | isNothing x = "." ++ printaRow xs
                | otherwise = (show . fromJust) x ++ printaRow xs

{-| Ex 2.2

    `readPuzzle f' reads from the FilePath `f', and either delivers it, or stops
    if `f' did not contain a puzzle. |-}
readPuzzle :: FilePath -> IO Puzzle
readPuzzle path =  do
					filetext  <-  readFile  path
					
					return (parsePuzzle filetext)

parsePuzzle :: String -> Puzzle
parsePuzzle s
	| not (isPuzzle (parse s)) = error "parsePuzzle: Not a sudoku."
	| otherwise                = parse s where
			parse =  Puzzle . (map (map f)) . lines where
						f '.' = Nothing
						f ch  = Just (digitToInt ch)
{-| Ex 3.1

    Check that a block contains no duplicate values. |-}
isValidBlock :: Block -> Bool
isValidBlock []   =  True
isValidBlock (Nothing:xs) = isValidBlock xs
isValidBlock (x:xs) = (notElem x xs) && (isValidBlock xs)
{-| Ex 3.2

    Collect all blocks on a board - the rows, the columns and the squares. |-}
blocks :: Puzzle -> [Block]
blocks puzzle = rows puzzle ++ (transpose . rows) puzzle ++ squares puzzle

squares :: Puzzle -> [Block]
squares = map concat .
           concatMap transpose . chunksOf 3 . map (chunksOf 3) . rows


{-| Ex 3.3

    Check that all blocks in a puzzle are legal. |-}
isValidPuzzle :: Puzzle -> Bool
isValidPuzzle = all isValidBlock . blocks

{-| Ex 4.1

    Given a Puzzle that has not yet been solved, returns a position in
    the Puzzle that is still blank. If there are more than one blank
    position, you may decide yourself which one to return. |-}
blank :: Puzzle -> Pos
blank puzzle = let xs = map (findIndex isNothing) (rows puzzle)
                   y = findIndex isJust xs
               in case y of
                    Just n -> Pos (n, fromJust ((!!) xs n))
 

{-| Ex 4.2

    Given a list, and a tuple containing an index in the list and a
    new value, updates the given list with the new value at the given
    index. |-}
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) = undefined

{-| Ex 4.3

    `update s p v' returns a puzzle which is a copy of `s' except that
    the position `p' is updated with the value `v'. |-}
update :: Puzzle -> Pos -> Maybe Int -> Puzzle
update = undefined

{-| Ex 5.1

    Solve the puzzle. |-}
solve :: Puzzle -> Maybe Puzzle
solve = undefined

{-| Ex 5.2

    Read a puzzle and solve it. |-}
readAndSolve :: FilePath -> IO (Maybe Puzzle)
readAndSolve path = readPuzzle path >>= maybe (print "(no solution)") printPuzzle . solve

{-| Ex 5.3

    Checks if s1 is a solution of s2. |-}
isSolutionOf :: Puzzle -> Puzzle -> Bool
isSolutionOf s1 s2 =  isValidPuzzle s1
                       && isSolved s1
                       && all (\(x, y) -> isNothing y || (x == y))
                            (zip ((concat . rows) s1) ((concat . rows) s2))

