-- imports
import Data.Char
import Data.List
import System.IO

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

-- defining a possible possition as a tuple with two positions (Int, Int) and a possible Bone
type Pos = (Int, Int)

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





posses :: Grid -> [Pos]
posses g = concat [positions (transpose g) 1, positions g (width g)]

allBones :: Grid -> [Bone]
allBones g = concat [map orderBone (zips (transpose g)), map orderBone (zips g)]

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

-- calculating width
width :: Grid -> Int
width g = length (head g)

positions :: Grid -> Int -> [Pos]
positions g p2start = zip p1 p2
                            where
                                p1 = [0..((width g) * (length g - 1))-1]
                                p2 = [p2start..]


-- elemIndices (_,_,(0,0)) (possBones startGrid)
-- determine the number of a bone           ORDERING MIGHT BE DONE OUTSIDE
boneNum :: Bone -> Int
boneNum (b1, b2) | b1 <= b2  = sum [maxBone+1-b1..maxBone] + b2 + 1
                 | otherwise = boneNum (b2, b1)

-- startBones :: [Bone]
-- startBones = zip [1..] (zip b1 b2)