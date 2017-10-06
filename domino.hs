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

-- defining a Bone as a tuple of two Ints
type Bone = (Int, Int)

-- defining a location as a tuple with two positions (Int, Int) and a possible Bone
type Loc = (Int, Int)

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




-- determine all location combinations on the grid
locsBones :: Grid -> [Loc]
locsBones g = concat [locations (transpose g) 1, locations g (width g)]

locations :: Grid -> Int -> [Loc]
locations g p2start = zip p1 p2
                            where
                                p1 = [0..((width g) * (length g - 1))-1]
                                p2 = [p2start..]

-- calculating width
width :: Grid -> Int
width g = length (head g)

-- determine all possible bones on the grid
possBones :: Grid -> [Bone]
possBones g = concat [map orderBone (zips (transpose g)), map orderBone (zips g)]

-- create bone for each horizontal neighbour
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






-- determine the number of a bone           ORDERING MIGHT BE DONE OUTSIDE
boneNum :: Bone -> Int
boneNum (b1, b2) = sum [maxBone+1-b1..maxBone] + b2 + 1





-- lists the number of occurences of all possible bones
occurences :: [Bone] -> [[Int]]
occurences possBones = [elemIndices u possBones | u <- nub possBones]     

sortLL :: [[Int]] -> [[Int]]
sortLL list = sortBy (comparing length) list

-- [[1,2,3],[3,4],[3,4,5,6]]
-- [(0,1),(0,1),(3,4),(5,6)]

play :: Grid -> IO ()
play g | length indices == 1    = putStrLn ("Er is een unieke op: " ++ show (locsBones g !! head(indices)))
       | otherwise              = putStrLn "Branchen met die handel"
         where indices = head (sortLL (occurences (possBones g)))
