import Data.List;
import System.IO;
import System.Process;
import Control.Applicative;
import Data.List.Split;
import System.Environment;
import Data.Char;

readInt:: String -> Int
readInt = read

-- cartesean product of a list with itself
cross list = [(x,y) | x <- list, y <- list]

-- Sudoku cells values are represented by triples where the first and second
-- entry specify row and column respectively (zero indexed) and the third
-- specifies labelling (1-9).
-- Define a bijection between cell value triples and natural numbers that will
-- serve as boolean variable names.
cellToVar (i,j,k) = fromIntegral $ (k-1)*81 + i*9 + j + 1
varToCell x = ((i `mod` 81) `div` 9, i `mod` 9, (i `div` 81)+1)
	where i = (fromIntegral x)-1

-- List of clauses that ensures a given cell is labeled with exactly one value.
-- Checks for every pair of labels that the cell is NOT labeled by both
-- and that the cell is is labeled with at least one value.
oneLabel (i,j) = atLeastOne : lessThan2
	where notBoth (c1,c2) = [- cellToVar (i,j,c1), - cellToVar (i,j,c2)]
              lessThan2  = map notBoth $ [(i,j) | (i,j) <- cross [1..9], i /= j]
              atLeastOne = map cellToVar [(i,j,k) | k <- [1..9]]

-- List of clauses that ensures every cell has exactly one label.
validLabeling = foldr ((++).oneLabel) [] $ cross [0..8]

-- Definition: A group of cells is a set of cells that must contain
-- one of all the labels. i.e. One of the square, columns or rows.
-- List of the square groups of cells.
squareGroups = [quadrent i j | (i,j) <- cross [0..2]]
	where quadrent x y = [(x*3+i,y*3+j) | (i,j) <- cross [0..2]]
-- List of rows, list of cols.
rows = [[(i,j) | i <- [0..8]] | j <- [0..8]]
cols = [[(i,j) | j <- [0..8]] | i <- [0..8]]

-- Formula that ensures a group of cells contains at least one of all labels [1-9].
groupGood group = foldr ((:).label) [] [1..9]
	where label k = map cellToVar [(i,j,k) | (i,j) <- group ]

-- Formula ensuring a labeling is good.
-- A labeling is "good" if it satisfies the sudoku constraints, that is every
-- square, row and cell contains one of each label.
goodLabeling = foldr ((++).groupGood) [] (squareGroups ++ rows ++ cols) 

-- Produce a formula for a set of sudoku constraints - filled in cells,
-- for which a model describes a sudoku solution.
sudokuForm cells = validLabeling ++ goodLabeling ++ (map consClause cells)
	where consClause cell = [cellToVar cell]

-- Produce a DIMACS formatted cnf string from a cnf formula.
-- A cnf formula is represented list of list of integers, i.e list of clauses.
-- Variables are positive integers, -x => not x.
cnfToDIMACS cnf = header ++ (unlines $ map showClause cnf)
	where max = maximum $ map maximum cnf
              header = "p cnf " ++ (show max) ++ " " ++ (show $ length cnf) ++ "\n"
              showClause clause =
               (foldl1 (\a b -> a ++ " " ++ b)  (map show clause)) ++ " 0"

-- Run minisat on a given cnf formula.
-- Return a variable values as list of integers.
-- If list contains x => x = true, if list contains -x => x = false.
runMinisat cnf = do
	let dimacs = cnfToDIMACS cnf
	writeFile "sudoku.cnf" dimacs
	system "./minisat sudoku.cnf sudoku.out"
	vars <- processMinisatOut <$> readFile "sudoku.out"
	return vars

-- Process minisat output file into variable assignment return [] if unsat.
processMinisatOut f | head (lines f) /= "SAT" = []
                    | otherwise = vars
	where vars = delete 0 $ map readInt $ splitOn " " $ (lines f !! 1)

-- Convert variable assignments into a sudoku matrix.
modelToMatrix model = [[lookup i j | j <- [0..8]] | i <- [0..8]]
	where
        cells = map varToCell $ filter ((<) 0) model
        lookup i j = label $ head $ filter (\(a,b,_) -> a == i && b == j) cells
        label (_,_,c) = c

-- Get constraints from sudoku matrix.
getConstraints matrix = filter (\(_,_,a) -> a > 0) cells
	where flat = foldl1 (++) matrix
	      cells = zip3 [i `div` 9 | i <- [0..]] (cycle [0..8]) flat

-- Solve a sudoku matrix.
sudokuSolve matrix = do
	msatout <- runMinisat $ sudokuForm $ getConstraints matrix
	return $ modelToMatrix msatout

-- Given a sudoku string return a sudoku  matrix.
stringToMatrix string = map (map (\ a -> readInt [a])) $ lines string
-- Given a sudoku matrix return a nice string for viewing.
showMatrix grid = unlines $ map (foldr ((++).show) []) grid

-- Solve sudokus in file specified by first argument.
main = do 
	fileName <- head <$> getArgs
	contents <- lines <$> readFile fileName
	let pruned = filter (isDigit.head) contents
	solveList pruned

solveList [] = do return 0
solveList list = do
	let (puzzle, rest) = splitAt 9 list
	solution <- sudokuSolve $ stringToMatrix $ unlines puzzle
	putStrLn $ showMatrix solution
	solveList rest

{-
-- Project Euler solution.
main = do 
	fileName <- head <$> getArgs
	contents <- lines <$> readFile fileName
	let pruned = filter (isDigit.head) contents
	ans <- solveList pruned 0
	putStrLn $ show ans

solveList [] acc = do return acc
solveList list acc = do
	let (puzzle, rest) = splitAt 9 list
	solution <- sudokuSolve $ stringToMatrix $ unlines puzzle
	putStrLn $ showMatrix solution
	let firstRow = head solution
	let add = (firstRow !! 0) * 100 
                   + (firstRow !! 1) *10 + (firstRow !! 2)
	solveList rest (acc + add)
-}
