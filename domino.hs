-- imports
import Data.Char
import Data.List
import System.IO
import Data.Ord

-- defining Grid as a list of list of Ints
type Grid = [[Int]]

-- defining a location as a tuple with two points (Int, Int)
type Loc = (Int, Int)

-- defining a Bone as a tuple of two Ints
type Bone = (Int, Int)

-- defining a Position as a combination of a Location and the fitting Bone
type Pos = (Loc, Bone)

-- defining a resultset as a combination of possible Positions and solution Positions
type Res = ([Pos], [Pos])

-- defining a tree with all possible solutions
data Tree a = Node a [Tree a] deriving Show

-- clearing the screen
cls :: IO ()
cls = do putStr "\ESC[2J" -- control character for clearing screen
         
-- the goto method (not to be confused with the goto in struct. progr.)
goto :: (Int, Int) -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- starting grid
grid :: Grid
grid = [[6,6,2,6,5,2,4,1],
        [1,3,2,0,1,0,3,4],
        [1,3,2,4,6,6,5,4],
        [1,0,4,3,2,1,1,2],
        [5,1,3,6,0,4,5,5],
        [5,5,4,0,2,6,0,3],
        [6,0,5,3,4,2,0,3]]

-- the maximum value in the game
boneNums :: Int
boneNums = maximum (maximum grid) + 1

-- showing the grid
putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . map showRow

-- show a row
showRow :: [Int] -> [String]
showRow = beside . map showInt
            where beside = foldr1 (zipWith (++))

-- show an Int
showInt :: Int -> [String]
showInt i = ["   ", " " ++ sizedInt ++ " ", "   "]
            where sizedInt | i < 10    = " " ++ show i
                           | otherwise = show i

-- chop List of Ints into Grid
chop :: Int -> [Int] -> Grid
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

-- turn initial grid into a list of locations and bones and a solution list
initiate :: Grid -> Res
initiate g = (concat [(zip lv bv), (zip lh bh)],[])
             where
              lv = filter (\(x,y) -> (x+1) `mod` (width g) /= 0) (zip [0..] [1..])
              lh = zip [0..] [(width g)..]
              bv = map orderBone (concat [ziph gr | gr <- g])
              bh = map orderBone (zipv g)

-- calculating width
width :: Grid -> Int
width g = length (head g)

-- create bone for each vertical neighbour
zipv :: Grid -> [Bone]
zipv [x] = []
zipv g   = concat [row, rest]
           where
            row  = zip (head g) (head (tail g))
            rest = zipv (tail g)

-- creating horizontal bones
ziph :: [Int] -> [(Int, Int)]
ziph [_]    = []
ziph (g:gs) = (g, (head gs)) : ziph gs
            
-- order bone low-high
orderBone :: Bone -> Bone
orderBone (b1, b2) | b1 <= b2  = (b1, b2)
                   | otherwise = (b2, b1)

-- determine the number of a bone
boneNum :: Bone -> Int
boneNum (b1, b2) = sum [boneNums-b1..boneNums-1] + b2 + 1

-- determine the number of bones
numBones :: Int -> Int
numBones n = n * (n + 1) `div` 2

-- start playing
play :: Grid -> IO ()
play g = do cls
            goto (1,1)
            putStrLn (concat (replicate (4 * width g) "*") ++ "\n")
            putStrLn "Grid:\n"
            putGrid g
            putStrLn (concat (replicate (4 * width g) "*") ++ "\n")
            putStrLn "Solutions:\n"
            putStrLn (concat (replicate (4 * width g) "=") ++ "\n")
            printSolutions (solutions2Grids (getResults (resultTree (initiate g))) (width g)) 0

-- logic for solving the puzzle
solve :: Res -> [Res]
solve (ps,ss) | ready (ps,ss)       = [(ps,ss)] -- all bones are placed, so we're ready
              | noSolution (ps,ss)  = []
              | not (null us)       = solve ((locFilter (doubleFilter (ps,(concat [ss,us]))) (posIndices us)),(concat [ss,us]))
              | not (null ns)       = solve ((locFilter (doubleFilter (ps,(concat [ss,ns]))) (posIndices ns)),(concat [ss,ns]))
              | otherwise           = [((locFilter (doubleFilter (ps,(concat [ss,[t]]))) (posIndices [t])),(concat [ss,[t]])) | t <- ts]
                where
                  us = getPosses ps (uniques (occurrences ps))
                  ns = posOneNeighbour ps
                  ts = getPosses ps (head (sortByLength (occurrences ps)))

-- creating a tree
resultTree :: Res -> Tree Res
resultTree sps = Node sps [resultTree sps' | sps' <- solve sps]

-- getting the results
getResults :: Tree Res -> [Res]
getResults (Node sps ns) | ready sps = [sps]
                         | otherwise = concat [getResults n | n <- ns]

-- check if ready
ready :: Res -> Bool
ready (ps,ss) = length ss == numBones boneNums

noSolution :: Res -> Bool
noSolution (ps,ss) = (ps == []) && (length ss < numBones boneNums)

-- filters out doubles that are already in the solutions list
doubleFilter :: Res -> [Pos]
doubleFilter (ps,ss) = filter ((`notElem` se) . snd) ps
                       where se = [snd s | s <- ss]

-- lists the number of occurrences of all possible bones and returns their indices
occurrences :: [Pos] -> [[Int]]
occurrences ps = [elemIndices u possBones | u <- nub possBones]
                where possBones = [snd p | p <- ps]

-- sort a list of lists by length
sortByLength :: [[Int]] -> [[Int]]
sortByLength list = sortBy (comparing length) list

-- get Pos based on the index
getPosses :: [Pos] -> [Int] -> [Pos]
getPosses ps is = [ps !! i | i <- is]

-- search list for uniques
uniques :: [[Int]] -> [Int]
uniques occs = [head x | x <- occs, length x == 1]

-- checks witch position has a location occurring just once
posOneNeighbour :: [Pos] -> [Pos]
posOneNeighbour ps = nub (concat [filt ps is])
                     where
                      is = [x | x <- (posIndices ps), count x (posIndices ps) == 1]
                      filt ps is = filter ((\(x,y) -> ((elem x is) || (elem y is))).fst) ps

-- count occurrences
count :: Int -> [Int] -> Int
count x xs = length (filter (==x) xs)

-- gives a list of indices of positions
posIndices :: [Pos] -> [Int]
posIndices ps = concat (map (\(x,y) -> [x,y]) ls)
                where ls = [l | (l,b) <- ps]

-- filters a list of positions with a list of location indices
locFilter :: [Pos] -> [Int] -> [Pos]
locFilter ps is = nub (concat [filt ps is])
                  where filt ps is = filter ((\(x,y) -> not (elem x is) && not (elem y is)).fst) ps

-- print all solutions
printSolutions :: [Grid] -> Int -> IO ()
printSolutions [] i     = putStrLn ("Finished with " ++ show i ++ " solutions.\n")
printSolutions (g:gs) i = do putGrid g
                             putStrLn (concat (replicate (4 * width g) "-") ++ "\n")
                             printSolutions gs (i+1)

-- transform a list of Results to a list of Grids
solutions2Grids :: [Res] -> Int -> [Grid]
solutions2Grids [] w   = []
solutions2Grids sols w = [(createSolutionGrid (pos2sols (snd s)) w) | s <- sols]

-- create a Grid from a list with solutions
createSolutionGrid :: [(Int, Int)] -> Int -> Grid
createSolutionGrid sol w = chop w [v | (_,v) <- (sortBy (comparing fst) sol)]

-- translate Pos to a solution list containing an index and a bone number
pos2sols :: [Pos] -> [(Int, Int)]
pos2sols ps = zip (calcIndices ps) (calcBones ps)

-- calculating indices of a Pos
calcIndices :: [Pos] -> [Int]
calcIndices []     = []
calcIndices (p:ps) = fst (fst p) : snd (fst p) : calcIndices ps

-- calculating number of a Pos
calcBones :: [Pos] -> [Int]
calcBones []     = []
calcBones (p:ps) = boneNum (snd p) : boneNum (snd p) : calcBones ps