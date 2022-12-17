{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Main where

import Data.Bifunctor (Bifunctor)
import Data.List (elemIndex, transpose)
import Data.List.NonEmpty (xor)
import Data.Maybe (fromJust, fromMaybe)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Graphics.Gloss
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss.Interface.IO.Game (Event (EventKey, EventMotion), Key (MouseButton), KeyState (Up), MouseButton (LeftButton))
import System.Environment (setEnv)

class Drawable a where
  draw :: State -> a -> Picture

data PieceColor = Black | White | NoColor deriving (Show, Eq, Ord)

data Piece = Piece Int PieceColor deriving (Show, Eq)

data TileValue = TileValue Piece | Empty deriving (Show, Eq)

data Tile = Tile (Int, Int) TileValue deriving (Show, Eq)

instance Drawable Piece where
  draw state (Piece index Black) = piecePictures state !! (index + 6)
  draw state (Piece index White) = piecePictures state !! index
  draw state (Piece index NoColor) = Blank

instance Drawable TileValue where
  draw _ Empty = Blank
  draw state (TileValue piece) = draw state piece

instance Drawable Tile where
  draw state (Tile (row, col) tileVal) =
    translate (calcTranslation col) (calcTranslation row) $
      pictures
        [ color finalColor $ sqaureSolid tileSize,
          draw state tileVal
        ]
    where
      State {selectedTile = st, hoveredPos = ht} = state
      basicColor = if even (row + col) then color1 else color2
      validPoints
        | st == (-1, -1) = []
        | otherwise = checkFreeMoves state

      finalColor
        | (row, col) == st = addColors (makeColor 1 0 0 0.25) basicColor
        | (row, col) == ht = bright basicColor
        | getTileAt state (row, col) `elem` validPoints = addColors (makeColor 0 1 0 0.25) basicColor
        | otherwise = basicColor
      calcTranslation n = toFloat n * tileSize + tileSize / 2

instance Drawable State where
  draw state _ = pictures [boardPicture, translate (-500) 0 $ Text ("row:" ++ show hx ++ "  col: " ++ show hy)]
    where
      boardPicture =
        translate
          (- windowWidth / 2)
          (- windowHeight / 2)
          $ pictures $
            map (draw state) $ board state
      State {selectedTile = (x, y), hoveredPos = (hx, hy)} = state

type Board = [Tile]

data State = State
  { board :: Board,
    selectedTile :: (Int, Int),
    hoveredPos :: (Int, Int),
    piecePictures :: [Picture],
    turn :: PieceColor,
    checkFreeMoves :: [Tile]
  }
  deriving (Show)

switchTurn :: PieceColor -> PieceColor
switchTurn NoColor = NoColor
switchTurn Black = White
switchTurn White = Black

getTileColor :: Tile -> PieceColor
getTileColor (Tile _ Empty) = NoColor
getTileColor (Tile _ (TileValue (Piece _ c))) = c

getTilePoint :: Tile -> (Int, Int)
getTilePoint (Tile pos _) = pos

map2d :: (a -> b) -> [[a]] -> [[b]]
map2d f = map (map f)

pointMul :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
pointMul (x', y') (x, y) = (x * x', y * y')

pointMulScalar :: Num b => b -> (b, b) -> (b, b)
pointMulScalar n p = pointMul p (n, n)

flattern :: [[a]] -> [a]
flattern arr = [elm | row <- arr, elm <- row]

chunks :: Int -> [a] -> [[a]]
chunks n [] = []
chunks n arr = take n arr : chunks n (drop n arr)

removeDups :: Eq a => [a] -> [a]
removeDups = rdHelper []
  where
    rdHelper seen [] = seen
    rdHelper seen (x : xs)
      | x `elem` seen = rdHelper seen xs
      | otherwise = rdHelper (seen ++ [x]) xs

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x : xs)
  | n == 0 = newVal : xs
  | otherwise = x : replaceNth (n -1) newVal xs

sqaureSolid :: Float -> Picture
sqaureSolid f = rectangleSolid f f

toFloat :: Integral a => a -> Float
toFloat x = fromIntegral x :: Float

inRange :: Ord a => a -> a -> a -> Bool
inRange min max n = max >= n && min <= n

isValidPosition :: (Int, Int) -> Bool
isValidPosition (x, y) = inRange 0 7 x && inRange 0 7 y

initialIntBoard :: [[Int]]
initialIntBoard =
  [ [2, 4, 3, 1, 0, 3, 4, 2],
    replicate 8 (-1)
  ]
    ++ replicate 4 (replicate 8 (-1))
    ++ map2d (+ 6) [replicate 8 (7), [2, 4, 3, 1, 0, 3, 4, 2]]

getPieceColor :: (Ord a, Num a) => a -> PieceColor
getPieceColor n = if n < 6 then White else Black

getTileInt :: Tile -> Int
getTileInt (Tile _ (TileValue (Piece i _))) = i
getTileInt (Tile _ Empty) = -1

-- hasPawnStarted (Tile _ Empty) = False
-- hasPawnStarted (Tile _ TileValue (Piece 5)) = False

initialBoard :: Board
initialBoard = [Tile (j, i) (initialSetup !! j !! i) | i <- [0 .. 7], j <- [0 .. 7]]
  where
    initialSetup = map2d intToTileValue initialIntBoard
    intToTileValue n = if n == -1 then Empty else TileValue $ Piece (n `mod` 6) (getPieceColor n)

initialState :: State
initialState = State {board = initialBoard, selectedTile = (-1, -1), hoveredPos = (-1, -1), piecePictures = [], turn = Black, checkFreeMoves = []}

color1 :: Color
color1 = makeColorI 238 217 202 255

color2 :: Color
color2 = makeColorI 97 61 61 255

tileSize = 75

noOfRows = 8

noOfCols = 8

windowWidth = tileSize * noOfCols

windowHeight = tileSize * noOfRows

-- drawTile :: Float -> Float -> Picture
-- drawTile x y = translate (x * tileSize + (tileSize / 2)) (y * tileSize + tileSize / 2) $ color tileColor $ rectangleSolid tileSize tileSize
--   where
--     tileColor = if even (round (x + y)) then color1 else color2

drawTile :: Tile -> State -> Picture
drawTile (Tile (x, y) _) state = rectanglePicture
  where
    State {selectedTile = st, hoveredPos = ht} = state
    basicColor = if even (x + y) then color1 else color2
    finalColor
      | (x, y) == st = green
      | (x, y) == ht = bright basicColor
      | otherwise = basicColor

    rectanglePicture = color finalColor $ rectangleSolid tileSize tileSize

-- piecePicture = drawTileValue

window :: Display
window = InWindow "Nice Window" (round $ tileSize * 8, round $ tileSize * 8) (100, 100)

background :: Color
background = white

drawing :: Picture
drawing = Blank

display' :: IO ()
display' = display window background drawing

getTileAt :: State -> (Int, Int) -> Tile
getTileAt State {board = board} (x, y) = head $ filter (\(Tile pos _) -> pos == (x, y)) board

posToTileIndex :: (Integral a, Integral a) => (Float, Float) -> (a, a)
posToTileIndex (x, y) = (toIndex x, toIndex y)
  where
    toIndex = floor . (/ tileSize) . (+ (tileSize * 4))

validateSelection :: State -> Tile -> Bool
validateSelection state (Tile _ Empty) = False
validateSelection State {turn = turn} (Tile _ (TileValue (Piece _ color))) = color == turn

-- getValidMoves :: State -> Tile -> [(Int, Int)]
-- getValidMoves state  =

validateMove :: State -> Tile -> Tile -> Bool
validateMove state t1 t2 = t2 `elem` getCheckFreeMoves state t1

takeWhileWithLast :: (a -> Bool) -> [a] -> [a]
takeWhileWithLast f arr = res ++ ([arr !! resLength | resLength /= length arr])
  where
    res = takeWhile f arr
    resLength = length res

type FilterArrType a = (a -> Bool) -> [a] -> [a]

pointToColor :: State -> (Int, Int) -> PieceColor
pointToColor state p = getTileColor $ getTileAt state p

getCellsInDir :: State -> (Int, Int) -> FilterArrType (Int, Int) -> (Int, Int) -> [(Int, Int)]
getCellsInDir state (x, y) filter2Func (dx, dy) = points
  where
    State {board = board, turn = turn} = state
    infiniteList = [(x + dx * i, y + dy * i) | i <- [1 ..]]
    -- Helper function
    tile = getTileAt state (x, y)

    points =
      takeWhileWithLast (\p -> getTileColor (getTileAt state p) /= switchTurn (getTileColor tile)) $
        takeWhile (\p -> getTileColor (getTileAt state p) /= getTileColor tile) $
          takeWhile isValidPosition infiniteList

getCellsInDirs :: State -> (Int, Int) -> [(Int, Int)] -> Int -> [(Int, Int)]
getCellsInDirs state (x, y) dirs limit = flattern $ map (take limit . getCellsInDir state (x, y) takeWhileWithLast) dirs

makeDirCombs :: Num b => Eq b => (b, b) -> [(b, b)]
makeDirCombs (x, y) = removeDups (basicDirs ++ map (pointMulScalar (-1)) basicDirs)
  where
    basicDirs = [(x, y), (y, x), (- x, y), (y, - x)]

makeDirsCombs :: (Num b, Eq b) => [(b, b)] -> [(b, b)]
makeDirsCombs dirs = flattern $ map makeDirCombs dirs

getPieceCombs ::
  (Eq a1, Num a1, Num a2, Num b, Num c) =>
  a1 ->
  ( State -> (Int, Int) -> [(Int, Int)] -> Int -> [(Int, Int)],
    [(a2, b)],
    c
  )
getPieceCombs 0 = (getCellsInDirs, [(1, 1), (0, 1)], 1)
getPieceCombs 1 = (getCellsInDirs, [(1, 1), (0, 1)], 8)
getPieceCombs 2 = (getCellsInDirs, [(0, 1)], 8)
getPieceCombs 3 = (getCellsInDirs, [(1, 1)], 8)
getPieceCombs 4 = (getCellsInDirs, [(2, 1)], 1)
getPieceCombs n = (getCellsInDirs, [(0, 0), (0, 0)], 0)

getPawnInitialRow :: PieceColor -> Int
getPawnInitialRow White = 1
getPawnInitialRow Black = 6
getPawnInitialRow NoColor = 0

getPawnMoves :: State -> Tile -> [(Int, Int)]
getPawnMoves state (Tile (x, y) (TileValue (Piece 5 color))) = verticalMoves ++ diagonalMoves
  where
    moveDir = if color == Black then -1 else 1
    allVericalMoves = getCellsInDir state (x, y) takeWhile (moveDir, 0)
    verticalMoves
      | x == getPawnInitialRow color = take 2 allVericalMoves
      | otherwise = take 1 allVericalMoves
    getCellsInDir' = take 1 . getCellsInDir state (x, y) takeWhileWithLast
    diagonalMoves = filter (\p -> getTileColor (getTileAt state p) /= NoColor) (getCellsInDir' (moveDir, 1) ++ getCellsInDir' (moveDir, -1))
getPawnMoves state (Tile (x, y) (TileValue (Piece a color))) = []
getPawnMoves state (Tile (x, y) Empty) = []

getValidMoves :: State -> Tile -> [Tile]
getValidMoves state (Tile (x, y) Empty) = []
getValidMoves state (Tile (x, y) (TileValue (Piece 5 col))) = map (getTileAt state) $ removeDups $ getPawnMoves state (Tile (x, y) (TileValue (Piece 5 col)))
getValidMoves state (Tile (x, y) (TileValue (Piece n col))) = map (getTileAt state) $ removeDups $ cellFunc state (x, y) (makeDirsCombs dirs) movementLimit
  where
    (cellFunc, dirs, movementLimit) = getPieceCombs n

getCheckFreeMoves :: State -> Tile -> [Tile]
getCheckFreeMoves state (Tile (x, y) Empty) = []
getCheckFreeMoves state (Tile (x, y) (TileValue (Piece n col))) = filter (isCheckFreeMove state initialTile) validMoves
  where
    initialTile = getTileAt state (x, y)
    validMoves = getValidMoves state initialTile

move :: State -> Tile -> Tile -> State
move state t1 t2 = state {board = newBoard}
  where
    State {board = board} = state
    i1 = fromMaybe 0 $ elemIndex t1 board
    i2 = fromMaybe 0 $ elemIndex t2 board
    (Tile pos tileValue1) = t1
    (Tile pos2 _) = t2
    newBoard = replaceNth i1 (Tile pos Empty) $ replaceNth i2 (Tile pos2 tileValue1) board

validateAndMove :: State -> (Int, Int) -> (Int, Int) -> State
validateAndMove state p1 p2 = if validateMove state t1 t2 then (move state t1 t2) {turn = switchTurn $ turn state, selectedTile = (-1, -1)} else state
  where
    t1 = getTileAt state p1
    t2 = getTileAt state p2

getKingTile :: State -> PieceColor -> Tile
getKingTile State {board = board} color = head $ filter (\t -> getTileColor t == color && getTileInt t == 0) board

getAllAttacks :: State -> PieceColor -> [Tile]
getAllAttacks state color = removeDups $ flattern $ map (getValidMoves state) $ filter (\tile -> getTileColor tile == color) $ board state

isKingUnderCheck :: State -> PieceColor -> Bool
isKingUnderCheck state color = getKingTile state color `elem` getAllAttacks state (switchTurn color)

isCheckFreeMove :: State -> Tile -> Tile -> Bool
isCheckFreeMove state t1 t2 = not $ isKingUnderCheck (move state t1 t2) (getTileColor t1)

-- x = filter (\(Tile _ (TileValue (Piece p color)) ) baord state

handleTileClick :: State -> State
handleTileClick state
  | not (isValidPosition ht) = state
  | ht == st = state {selectedTile = (-1, -1)}
  | st == (-1, -1) = if isValidSelection then state {selectedTile = ht, checkFreeMoves = getCheckFreeMoves state (getTileAt state ht)} else state
  | otherwise =
    if
        | ht == st -> state {selectedTile = (-1, -1)}
        | hoveredTileColor == turn state -> state {selectedTile = ht}
        | otherwise -> validateAndMove state st ht
  where
    State {selectedTile = st, hoveredPos = ht} = state
    hoveredTileColor = getTileColor hoveredTile
    isValidSelection = validateSelection state hoveredTile
    hoveredTile = getTileAt state ht

transform :: Event -> State -> State
transform (EventKey (MouseButton LeftButton) Up _ (x, y)) state = handleTileClick state
transform (EventMotion (x, y)) state = state {hoveredPos = posToTileIndex (y, x)}
transform _ state = state

printState :: State -> IO ()
printState state =
  let rows = map show $ transpose $ chunks 8 $ board state
   in putStr $ unlines rows

loadPieceImages :: [IO Picture]
loadPieceImages =
  map (loadBMP . (\n -> "images/" ++ n ++ ".bmp") . (\n -> if n < 6 then show n ++ "-w" else show (n `mod` 6) ++ "-b")) [0 .. 11]

s :: State
s = initialState

main :: IO ()
main = do
  piecePictures <- sequence loadPieceImages
  play window (makeColor 1 0 0 1) 10000 (initialState {piecePictures = piecePictures}) (\s -> draw s s) transform (const id)

makePairs :: Integral a => [a] -> a -> Int
makePairs arr a = length $ filter (\t -> t `rem` a == 0) $ concat $ [map (\t -> t + (arr !! i)) $ take i arr | i <- [0 .. (length arr - 1)]]





solve = do
  l1 <- getLine
  l2 <- getLine
  let [_, k] = (map read $ words l1) :: [Int]
  let elms = (map read $ words l2) :: [Int]
  print $ makePairs elms k