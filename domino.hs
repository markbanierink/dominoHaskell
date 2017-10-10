-- imports
import Data.Char
import Data.List
import System.IO
import Data.Ord

-- clearing the screen
cls :: IO ()
cls = do putStr "\ESC[2J" -- control character for clearing screen
         
-- the goto method (not to be confused with the goto in struct. progr.)
goto :: (Int, Int) -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- defining Grid as a list of list of Ints
type Grid = [[Int]]

-- defining a location as a tuple with two points (Int, Int)
type Loc = (Int, Int)

-- defining a Bone as a tuple of two Ints
type Bone = (Int, Int)

-- defining a Position as a combination of a Location and the fitting Bone
type Pos = (Loc, Bone)

-- defining a Solution as a list of combinations of an index and a bone value
type Sol = [Pos]

-- the maximum value in the game
boneNums :: Int
boneNums = 7

-- starting grid
startGrid :: Grid
startGrid = [[6,6,2,6,5,2,4,1],
             [1,3,2,0,1,0,3,4],
             [1,3,2,4,6,6,5,4],
             [1,0,4,3,2,1,1,2],
             [5,1,3,6,0,4,5,5],
             [5,5,4,0,2,6,0,3],
             [6,0,5,3,4,2,0,3]]

-- showing the grid
putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . map showRow -- unlines joins a list of lines with newlines inbetween

showRow :: [Int] -> [String]
showRow = beside . map showInt
            where beside = foldr1 (zipWith (++)) -- foldr1 doesn't accept empty lists, zipwith zips and then applies function to each pair in the list

showInt :: Int -> [String]
showInt i = ["   ", " " ++ sizedInt ++ " ", "   "]
            where sizedInt | i < 10    = " " ++ show i
                           | otherwise = show i

interleave :: a -> [a] -> [a]
interleave x []     = []
interleave x [y]    = [y]
interleave x (y:ys) = y : x : interleave x ys

-- chop List of Ints into Grid
chop :: Int -> [Int] -> Grid
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)





-- turn initial grid into map of locations and bones
initial :: Grid -> [Pos]
initial g = concat [(zip lv bv), (zip lh bh)]
            where
              lv = zip [0..] [1..]
              lh = zip [0..] [(width g)..]
              bv = map orderBone (zips (transpose g))
              bh = map orderBone (zips g)

-- calculating width
width :: Grid -> Int
width g = length (head g)

-- create bone for each vertical neighbour
zips :: Grid -> [Bone]
zips [x] = []
zips g   = concat [row, rest]
           where
            row  = zip (head g) (head (tail g))
            rest = zips (tail g)

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





-- starting point of the program
play :: Grid -> IO ()
play g = do cls
            goto (1,1)
            putGrid g
            play' (initial g) (width g)

-- real start
play' :: [Pos] -> Int -> IO ()
play' ps w = printSolutions (solutions2Grids (solve ps []) w)

-- logic for solving the puzzle
solve :: [Pos] -> Sol -> [Sol]
solve ps ss | length ss == numBones boneNums = [ss] -- all bones are placed, so we're ready
            | length indices > 0             = solve (locFilter ps (posIndices (getPosses ps indices))) (concat [ss, (getPosses ps indices)])
            | length oneNeighbour > 0        = solve (locFilter ps oneNeighbour) (concat [ss, locFilter ps oneNeighbour])
            | otherwise                      = [ss] -- branchen!!!   [solve p ss | p <- ]
              where
                indices       = uniques (occurences ps)
                oneNeighbour  = occurOnce (posIndices ps)


-- -- defining a tree with all possible solutions
-- data Tree a = Node a [Tree a] deriving Show

-- -- we can create this very easily by recursion
-- gametree :: [Pos] -> [Pos] -> Tree Grid
-- gametree ps ss = Node ss [gametree ps ss' | ss' <- solve ps ss]


-- lists the number of occurences of all possible bones and returns their indices
occurences :: [Pos] -> [[Int]]
occurences ps = [elemIndices u possBones | u <- nub possBones]
                 where possBones = [snd p | p <- ps]

-- -- sort a list of lists by length
-- sortByLength :: [[Int]] -> [[Int]]
-- sortByLength list = sortBy (comparing length) list

-- get Pos based on the index
getPosses :: [Pos] -> [Int] -> [Pos]
getPosses ps is = [ps !! i | i <- is]

-- addPosSol :: [Grid] -> [Pos] -> [Grid]
-- addPosSol gs ps = p | p <- ps
--                   where g = 

-- search list for uniques
uniques :: [[Int]] -> [Int]
uniques occs = [head x | x <- occs, length x == 1]



-- finds the locations that only occur once
occurOnce :: Eq a => [a] -> [a]
occurOnce xs = [x | x <- xs, count x xs == 1]

count :: Eq a => a -> [a] -> Int
count x xs = length (filter (==x) xs)

-- gives a list of indices of a pos
posIndices :: [Pos] -> [Int]
posIndices ps = concat (map (\(x,y) -> [x,y]) ls)
                where ls = [l | (l,b) <- ps]

-- filters a list of positions with a list of location indices
locFilter :: [Pos] -> [Int] -> [Pos]
locFilter _ []  = []
locFilter ps ls = filter ((\(x,y) -> x /= l && y /= l).fst) (locFilter ps (tail ls))
                  where l = head ls

locFilterPstv :: [Pos] -> [Int] -> [Pos]
locFilterPstv _ []  = []
locFilterPstv ps ls = filter ((\(x,y) -> x == l || y == l).fst) (locFilterPstv ps (tail ls))
                      where l = head ls
-- uni :: [(Int, Int)] -> [Int]
-- uni (ls,bs) = concat (map (\(x,y) -> [x,y]) ls)


-- (initial startGrid) !! (head (uniques (occurences (initial startGrid))))

-- Filters positions leaving locations given as an argument out
-- filterLocations :: [Pos] -> Loc -> [Pos]
-- filterLocations ps r = [p | p <- ps, locationFilter p r]

-- locationFilter :: Pos -> Loc -> Bool
-- locationFilter (ls,bs) (r1,r2) = l1 /= r1 && l2 /= r1 && l1 /= r2 && l2 /= r2
--                                  where
--                                     l1 = fst ls
--                                     l2 = snd ls

-- filterNeighbours :: [Pos] -> Loc -> [Pos]
-- filterNeighbours ps r = [p | p <- ps, neighboursFilter p r]

-- neighboursFilter :: Pos -> Loc -> Bool
-- neighboursFilter (ls,bs) (r1,r2) = l1 /= r1 && l2 /= r1 && l1 /= r2 && l2 /= r2
--                                   where
--                                     l1 = fst ls
--                                     l2 = snd ls

-- neighboursFilter' :: Pos -> [Int] -> Bool
-- neighboursFilter' (ls,bs) rs = l1 /= r && l2 /= r
--                                    where
--                                     l1 = fst ls
--                                     l2 = snd ls
--                                     r <- rs



printSolutions :: [Grid] -> IO ()
printSolutions []     = putStrLn "All solutions printed"
printSolutions (g:gs) = do putGrid g
                           putStrLn (concat (replicate (4 * width g) "=") ++ "\n")
                           printSolutions gs

solutions2Grids :: [[Pos]] -> Int -> [Grid]
solutions2Grids sols w = [(createSolutionGrid (pos2sols s) w) | s <- sols]

pos2sols :: [Pos] -> [(Int, Int)]
pos2sols ps = zip i v
              where 
                i = [fst (fst p) | p <- ps]
                v = [boneNum (snd p) | p <- ps]

createSolutionGrid :: [(Int, Int)] -> Int -> Grid
createSolutionGrid sol w = chop w [v | (_,v) <- (sortBy (comparing fst) sol)]






-- neighbours can be found by looking for overlapping positions.
-- if a possible bone has no overlapping positions with other possible bones, it has no neighbours

-- [[1,2,3],[3,4],[3,4,5,6]]
-- [(0,1),(0,1),(3,4),(5,6),(3,5)]
-- putGrid (chop (width startGrid) (concat startGrid))


-- filteredPossBones :: [Pos] -> Loc -> [Pos]
-- filteredPossBones ((l1,l2),(b1,b2)) (r1,r2) = filter (possBoneFilter (l1,l2) (r1,r2)) ((l1,l2),(b1,b2))