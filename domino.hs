-- imports
import Data.Char
import Data.List
import System.IO
import Data.Ord

-- type Pos = (Int, Int)

-- clearing the screen
cls :: IO ()
cls = putStr "\ESC[2J" -- control character for clearing screen

-- the goto method (not to be confused with the goto in struct. progr.)
-- goto :: Pos -> IO ()
-- goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- defining Grid as a list of list of Ints
type Grid = [[Int]]

-- defining a location as a tuple with two points (Int, Int)
type Loc = (Int, Int)

-- defining a Bone as a tuple of two Ints
type Bone = (Int, Int)

-- defining a Position as a combination of a Location and the fitting Bone
type Pos = (Loc, Bone)

-- the maximum value in the game
maxBone :: Int
maxBone = 6

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
boneNum (b1, b2) = sum [maxBone+1-b1..maxBone] + b2 + 1

-- lists the number of occurences of all possible bones
occurences :: [Pos] -> [[Int]]
occurences ps = [elemIndices u possBones | u <- nub possBones]
                 where possBones = [snd p | p <- ps]

-- sort a list of lists by length
sortByLength :: [[Int]] -> [[Int]]
sortByLength list = sortBy (comparing length) list






play :: Grid -> IO ()
play g = play' (initial g)

play' :: [Pos] -> IO ()
play' ps | length indices == 1 = putStrLn ("Er is een unieke op: " ++ show (ps !! head indices))
         | otherwise           = putStrLn "Branchen met die handel"
           where indices = head (sortByLength (occurences ps))

-- neighbours can be found by looking for overlapping positions.
-- if a possible bone has no overlapping positions with other possible bones, it has no neighbours

-- [[1,2,3],[3,4],[3,4,5,6]]
-- [(0,1),(0,1),(3,4),(5,6)]
-- putGrid (chop (width startGrid) (concat startGrid))


-- filteredPossBones :: [Pos] -> Loc -> [Pos]
-- filteredPossBones ((l1,l2),(b1,b2)) (r1,r2) = filter (possBoneFilter (l1,l2) (r1,r2)) ((l1,l2),(b1,b2))
-- Filters positions leaving locations given as an argument out
filteredPossBones :: [Pos] -> Loc -> [Pos]
filteredPossBones ps r = [p | p <- ps, possBoneFilter p r]

possBoneFilter :: Pos -> Loc -> Bool
possBoneFilter (ls,bs) (r1,r2) = l1 /= r1 && l2 /= r1 && l1 /= r2 && l2 /= r2
                                 where
                                    l1 = fst ls
                                    l2 = snd ls
