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

-- defining a resultset as a combination of possible Positions and solution Positions
type Res = ([Pos], [Pos])

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
ziph [_] = []
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

-- -- initialising
-- play :: Grid -> IO ()
-- play g = do cls
--             goto (1,1)
--             putGrid g
--             putStrLn "Finding solutions...\n"
--             putStrLn (concat (replicate (4 * width g) "=") ++ "\n")
--             play' (initiate g) (width g)

-- -- start finding solutions
-- play' :: [Pos] -> Int -> IO ()
-- play' ps w = printSolutions (solutions2Grids (solve ps []) w)

-- logic for solving the puzzle
solve :: Res -> [Res]
solve (ps,ss) | ready (ps,ss) = [(ps,ss)] -- all bones are placed, so we're ready
              | not (null us) = solve ((locFilter ps (posIndices us)),(concat [ss, us]))
              | not (null ns) = solve ((locFilter ps (posIndices ns)),(concat [ss, ns]))
              | otherwise     = [((locFilter ps (posIndices [t])),(concat [ss, [t]])) | t <- ts]
                where
                  db = doubleFilter ps ss
                  us = getPosses db (uniques (occurences db))
                  ns = posOneNeighbour db
                  ts = getPosses db (head (sortByLength (occurences db)))
                  -- ts = getPosses db ((sortByLength (occurences db)) !! 0)

-- defining a tree with all possible solutions
data Tree a = Node a [Tree a] deriving Show

-- creating a tree
resultTree :: Res -> Tree Res
resultTree sps = Node sps [resultTree sps' | sps' <- solve sps]

tmp :: Tree Res -> [Res]
tmp (Node sps ns) = solve sps

-- getting the results
getResults :: Tree Res -> [Res]
getResults (Node sps ns) | ready sps = [sps]
                         | otherwise = concat [getResults n | n <- ns]

-- check if ready
ready :: Res -> Bool
ready (ps,ss) = length ss == numBones boneNums

-- filters out doubles that are already in the solutions list
doubleFilter :: [Pos] -> [Pos] -> [Pos]
doubleFilter ps ss = filter ((`notElem` se) . snd) ps
                     where se = [snd s | s <- ss]

-- lists the number of occurences of all possible bones and returns their indices
occurences :: [Pos] -> [[Int]]
occurences ps = [elemIndices u possBones | u <- nub possBones]
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

-- checks with position has a location occurring just once
posOneNeighbour :: [Pos] -> [Pos]
posOneNeighbour ps = nub (concat [filt ps is])
                     where
                      is = [x | x <- (posIndices ps), count x (posIndices ps) == 1]
                      filt ps is = filter ((\(x,y) -> ((elem x is) || (elem y is))).fst) ps

-- count occurences
count :: Eq a => a -> [a] -> Int
count x xs = length (filter (==x) xs)

-- gives a list of indices of positions
posIndices :: [Pos] -> [Int]
posIndices ps = concat (map (\(x,y) -> [x,y]) ls)
                where ls = [l | (l,b) <- ps]

-- filters a list of positions with a list of location indices
locFilter :: [Pos] -> [Int] -> [Pos]
locFilter ps is = nub (concat [filt ps is])
                  where filt ps i = filter ((\(x,y) -> not (elem x is) && not (elem y is)).fst) ps


-- print all solutions
printSolutions :: [Grid] -> IO ()
printSolutions []     = putStrLn "Finished."
printSolutions (g:gs) = do putGrid g
                           putStrLn (concat (replicate (4 * width g) "-") ++ "\n")
                           printSolutions gs

solutions2Grids :: [[Pos]] -> Int -> [Grid]
solutions2Grids [] w   = []
solutions2Grids sols w = [(createSolutionGrid (pos2sols s) w) | s <- sols]

pos2sols :: [Pos] -> [(Int, Int)]
pos2sols ps = zip (calcIndices ps) (calcBones ps)

calcIndices :: [Pos] -> [Int]
calcIndices []     = []
calcIndices (p:ps) = fst (fst p) : snd (fst p) : calcIndices ps

calcBones :: [Pos] -> [Int]
calcBones []     = []
calcBones (p:ps) = boneNum (snd p) : boneNum (snd p) : calcBones ps

createSolutionGrid :: [(Int, Int)] -> Int -> Grid
createSolutionGrid sol w = chop w [v | (_,v) <- (sortBy (comparing fst) sol)]

-- -- logic for solving the puzzle
-- solve :: [Pos] -> [Pos] -> [[Pos]
-- solve ps ss | ready ss            = [ss] -- all bones are placed, so we're ready
--             | hasUniques us       = solve (locFilter ps (posIndices us)) (concat [ss, us])
--             | hasOneNeighbours ns = solve (locFilter ps (posIndices ns)) (concat [ss, ns])
--             | otherwise           = [ss] -- [solve (locFilter ps (posIndices t)) (concat [ss, t]) | t <- ts]
--                where
--                 us = getPosses ps (uniques (occurences (doubleFilter ss ps)))
--                 ns = posOneNeighbour (doubleFilter ss ps)
--                 -- ts = getPosses ps (head (sortByLength (occurences (doubleFilter ss ps))))

-- [[((1,1),(1,1))],[((1,1),(1,1))]]
-- ps' = locFilter ps (posIndices (getPosses ps (uniques (occurences ps))))
-- ps'' = locFilter ps' (posIndices (posOneNeighbour ps'))


-- minimax :: Tree [Res] -> Tree ([Res], Bool)
-- minimax (Node sps _) | ready sps  = Node (sps, True) []
--                      | otherwise  = Node (sps, False) sps'
--                        where sps' = map minimax sps -- apply minimax to each child

-- play2 :: Grid -> IO ()
-- play2 g = do cls
--              goto (1,1)
--              putGrid g
--              putStrLn "Finding solutions...\n"
--              putStrLn (concat (replicate (4 * width g) "=") ++ "\n")
--              gametree (initiate g) []


-- print :: Tree [Pos] -> IO ()
-- print tree = do putGrid (createGrid tree 8)

-- createGrid :: Tree [Pos] -> Int -> Grid
-- createGrid tree w = 



-- -- testing grid
-- testGrid :: Grid
-- testGrid = [[6,6,2,6,5,2,4,1],
--             [1,3,2,0,1,0,3,4],
--             [1,3,2,4,6,6,5,5],
--             [1,0,4,3,2,1,1,2],
--             [5,1,3,6,0,4,5,5],
--             [5,5,4,0,2,6,0,3],
--             [6,0,5,3,4,2,1,3]]



-- ps' = locFilter ps (posIndices (getPosses ps (uniques (occurences ps))))
-- ps'' = locFilter ps' (posIndices (posOneNeighbour ps'))

-- posOneNeighbour (locFilter ps (posIndices (getPosses ps (uniques (occurences ps)))))

-- locFilterPstv :: [Pos] -> [Int] -> [Pos]
-- locFilterPstv ps is = nub (concat [filt ps i | i <- is])
--                       where filt ps i = filter ((\(x,y) -> x == i || y == i).fst) ps

-- tFilt :: [Pos] -> Int -> [Pos]
-- tFilt ps i = filter ((\(x,y) -> x == i || y == i).fst) ps
-- uni :: [(Int, Int)] -> [Int]
-- uni (ls,bs) = concat (map (\(x,y) -> [x,y]) ls)

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






-- (initial startGrid) !! (head (uniques (occurences (initial startGrid))))

-- fs = [((1,2),(2,3)),((3,4),(5,6)),((4,5),(1,2)),((5,6),(4,5))]
-- ss = [((1,2),(2,3)),((3,4),(1,6)),((4,5),(1,4)),((5,6),(1,3))]

-- x1 = [((0,1),(6,6)),((2,3),(1,3)),((4,5),(1,3)),((6,7),(0,1)),((8,9),(1,5)),((10,11),(5,5)),((12,13),(0,6))]
-- x2 = [((14,15),(2,6)),((16,17),(0,2)),((18,19),(2,4)),((20,21),(3,4)),((22,23),(3,6)),((24,25),(0,4)),((26,27),(3,5))]
-- x3 = [((28,29),(2,5)),((30,310),(0,1)),((32,33),(6,6)),((34,35),(1,2)),((36,37),(0,4)),((38,39),(2,6)),((40,41),(2,4))]
-- x4 = [((42,43),(1,4)),((44,45),(3,4)),((46,47),(4,5)),((48,49),(1,2)),((50,51),(5,5)),((52,53),(0,3)),((54,55),(0,3))]
-- x = concat [x1,x2,x3,x4]

-- x = [((12,13),(0,6)),((14,15),(2,6)),((16,17),(0,2)),((18,19),(2,4)),((20,21),(3,4)),((22,23),(3,6)),((24,25),(0,4)),((26,27),(3,5)),((28,29),(2,5)),((30,31),(0,1)),((32,33),(6,6)),((34,35),(1,2)),((36,37),(0,4)),((38,39),(2,6)),((40,41),(2,4)),((42,43),(1,4)),((44,45),(3,4)),((46,47),(4,5)),((48,49),(1,2)),((50,51),(5,5)),((52,53),(0,3)),((54,55),(0,3)),((0,1),(6,6)),((2,3),(1,3)),((4,5),(1,3)),((6,7),(0,1)),((8,9),(1,5)),((10,11),(5,5))]

-- [((0,1),(6,6)),((1,2),(1,3)),((2,3),(1,3)),((3,4),(0,1)),((4,5),(1,5)),((5,6),(5,5)),((6,7),(0,6)),
-- ((7,8),(2,6)),((8,9),(2,3)),((9,10),(2,3)),((10,11),(0,4)),((11,12),(1,3)),((12,13),(4,5)),((13,14)
-- ,(0,5)),((14,15),(2,6)),((15,16),(0,2)),((16,17),(2,4)),((17,18),(3,4)),((18,19),(3,6)),((19,20),(0,4))
-- ,((20,21),(3,5)),((21,22),(5,6)),((22,23),(0,1)),((23,24),(4,6)),((24,25),(2,3)),((25,26),(0,6)),((26,27),(0,2))
-- ,((27,28),(3,4)),((28,29),(2,5)),((29,30),(0,1)),((30,31),(6,6)),((31,32),(1,2)),((32,33),(0,4)),((33,34),(2,6))
-- ,((34,35),(2,4)),((35,36),(2,4)),((36,37),(0,3)),((37,38),(5,6)),((38,39),(1,1)),((39,40),(4,5)),((40,41),(0,6))
-- ,((41,42),(0,2)),((42,43),(1,4)),((43,44),(3,4)),((44,45),(4,5)),((45,46),(1,2)),((46,47),(5,5)),((47,48),(0,3))
-- ,((48,49),(0,3)),((0,8),(1,6)),((1,9),(3,6)),((2,10),(2,2)),((3,11),(0,6)),((4,12),(1,5)),((5,13),(0,2)),((6,14),
-- (3,4)),((7,15),(1,4)),((8,16),(1,1)),((9,17),(3,3)),((10,18),(2,2)),((11,19),(0,4)),((12,20),(1,6)),((13,21),(0,6)),
-- ((14,22),(3,5)),((15,23),(4,4)),((16,24),(1,1)),((17,25),(0,3)),((18,26),(2,4)),((19,27),(3,4)),((20,28),(2,6)),((21,29)
-- ,(1,6)),((22,30),(1,5)),((23,31),(2,4)),((24,32),(1,5)),((25,33),(0,1)),((26,34),(3,4)),((27,35),(3,6)),((28,36),(0,2)),
-- ((29,37),(1,4)),((30,38),(1,5)),((31,39),(2,5)),((32,40),(5,5)),((33,41),(1,5)),((34,42),(3,4)),((35,43),(0,6)),((36,44),
-- (0,2)),((37,45),(4,6)),((38,46),(0,5)),((39,47),(3,5)),((40,48),(5,6)),((41,49),(0,5)),((42,50),(4,5)),((43,51),(0,3)),((44,52),
-- (2,4)),((45,53),(2,6)),((46,54),(0,0)),((47,55),(3,3))]
-- neighbours can be found by looking for overlapping positions.
-- if a possible bone has no overlapping positions with other possible bones, it has no neighbours

-- [[1,2,3],[3,4],[3,4,5,6]]
-- [(0,1),(0,1),(3,4),(5,6),(3,5)]
-- putGrid (chop (width startGrid) (concat startGrid))


-- filteredPossBones :: [Pos] -> Loc -> [Pos]
-- filteredPossBones ((l1,l2),(b1,b2)) (r1,r2) = filter (possBoneFilter (l1,l2) (r1,r2)) ((l1,l2),(b1,b2))