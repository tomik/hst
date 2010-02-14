module Board where

import qualified Data.Map as Map
import Data.List

import Utils (slice, exportMaybes)

-- ==============================
--declarations and instancing
-- ==============================

--white connects top to bottom
--black connects left to right
data Color = Black | White deriving Eq

instance Show Color where
    show Black = "B"
    show White = "W"

--size is (maxrow + 1, maxcol + 1)
type Size = (Int, Int)
--pos is (row, col) where row|col is in [0, maxrow|maxcol -1]
type Pos = (Int, Int)

data Peg = Peg {pegPos :: Pos, pegColor :: Color} deriving (Eq)
type Pegs = [Peg]

instance Show Peg where 
    show peg = "[" ++ show (pegPos peg) ++ show (pegColor peg) ++ "]"

data Group = Group {grpColor:: Color, grpPegs :: [Peg]} deriving (Show, Eq)
type Groups = Map.Map Pos Group

data Board = Board {bdSize :: Size, bdPegs:: Pegs, bdToPlay :: Color, bdGroups :: Groups} deriving (Eq)

showSquare :: Board -> Pos -> String
showSquare board pos = let value = Map.lookup pos (bdGroups board)
                       in case value of
                       Nothing -> " . "
                       Just group -> " " ++ show (grpColor group) ++ " "

instance Show Board where
    show board = let (sizey, sizex) = bdSize board
                     line = [ showSquare board (row, col) | row <- [0, 1..(sizey - 1)], col <- [0, 1..(sizex - 1)] ]
                     header = "Board " ++ (show $ bdToPlay board) ++ " to play:"
                 in unlines $ [header] ++
                              (map concat $ slice sizex line) ++
                              ["Pegs: " ++ show (bdPegs board)] ++
                              ["Groups: " ++ show (bdGroups board)]
    
-- ==============================
--constructors and convenience functions
-- ==============================

--abs + rel pos -> pos
offsetPos :: Pos -> Pos -> Pos
offsetPos (posy, posx) (offy, offx) = (posy + offy, posx + offx)

oppColor :: Color -> Color
oppColor White = Black
oppColor Black = White

getRow :: Pos -> Int
getRow (row, col) = row

getCol :: Pos -> Int
getCol (row, col) = col

mkBoard :: Int -> Int -> Board
mkBoard sizey sizex = Board {
                        bdSize = (sizey, sizex),
                        bdPegs = [],
                        bdToPlay = White,
                        bdGroups = Map.empty
                        }

mkGroup :: Peg -> Group
mkGroup peg = Group{
                    grpColor=pegColor peg,
                    grpPegs=[peg]
                    }

mkPeg :: Int -> Int -> Color -> Peg
mkPeg posy posx color = Peg{pegPos=(posy, posx), pegColor=color}

mkPegs :: [(Int, Int, Color)] -> Pegs
mkPegs [] = []
mkPegs ((posy, posx, color):rest) = (mkPeg posy posx color):(mkPegs rest)

pegsByColor :: Pegs -> Color -> Pegs
pegsByColor pegs color = filter (\peg -> pegColor peg == color) pegs

whitePegs = flip pegsByColor White
blackPegs = flip pegsByColor Black

-- ==============================
--limitations
-- ==============================

isGoalEdge:: Size -> Pos -> Color -> Bool
isGoalEdge _ (0, _) White = True
isGoalEdge _ (_, 0) Black = True
isGoalEdge (sizey, _) (posy, _) White | sizey - 1 == posy = True
isGoalEdge (_, sizex) (_, posx) Black | sizex - 1 == posx = True
isGoalEdge _ _ _ = False

isEdge :: Size -> Pos -> Bool
isEdge size pos = (isGoalEdge pos size White) || (isGoalEdge pos size Black)

isOnBoard :: Board -> Peg -> Bool
isOnBoard board peg = getRow (pegPos peg) < getRow (bdSize board)
                      && getRow (pegPos peg) >= 0
                      && getCol (pegPos peg) < getCol (bdSize board)
                      && getCol (pegPos peg) >= 0

--on the board, not on the opponents edge and on the empty square
isLegalMove :: Board -> Peg -> Bool
isLegalMove board peg = isOnBoard board peg
                        && not (isGoalEdge (bdSize board) (pegPos peg) (oppColor $ pegColor peg))
                        && length (filter (\p -> (pegPos peg) == (pegPos p)) (bdPegs board)) == 0


-- ==============================
--bridge spoiling
-- ==============================

--transforming posPair in any position to posPair in referential position

rotateClockWise :: Pos -> Pos
rotateClockWise (row, col) = (col, -1 * row)

flipVertically :: Pos -> Pos
flipVertically (row, col) = (-1 * row, col)

--relative positions of cross pairs for bridge at referential position (2`oclock)
relSpoilPairs  = [((-2, 0), (0, 1)), ((-1, -1), (0, 1)), ((-1, 0), (1, 1)), ((1, 0), (-1, 1)),
                  (( 1, 2), (-1, 1)), ((0, 3), (-1, 1)), ((0, 2), (-2, 1)), ((0, 1), (-2, 2))]

--for a potential bridge (pair of positions) and processing function gives positions which can spoil it
--processor handles transformations on relSpoilPairs arising from use of non-referential pair orientation
doGenSpoilPairs :: (Pos -> Pos) -> (Pos, Pos) -> [(Pos, Pos)]
doGenSpoilPairs processor (pos1@(pos1y, pos1x), pos2@(pos2y, pos2x))
    = let processed = map (\(off1, off2) -> (processor off1, processor off2)) relSpoilPairs
      in map (\(off1, off2) -> (offsetPos pos1 off1, offsetPos pos1 off2)) processed

--wrapper around doGenSpoilPairs
genSpoilPairs :: (Pos, Pos) -> [(Pos, Pos)]
genSpoilPairs posPair@((pos1y, pos1x), (pos2y, pos2x))
    --pos1 should be more on the left
    | pos1x >= pos2x = genSpoilPairs ((pos2y, pos2x), (pos1y, pos1x))
    --1 o`clock
    | pos1y - pos2y == 2 = doGenSpoilPairs (flipVertically . rotateClockWise) posPair
    --2 o`clock - referential position
    | pos1y - pos2y == 1 = doGenSpoilPairs id posPair
    --4 o`clock
    | pos2y - pos1y == 1 = doGenSpoilPairs flipVertically posPair
    --5 o`clock
    | pos2y - pos1y == 2 = doGenSpoilPairs rotateClockWise posPair
    | otherwise = error "No matching pos pair position"

--peg-level wrapper for genSpoilPairs 
dropSpoilPegs :: Peg -> Pegs -> (Pos->Pos->Bool) -> Pegs
--signature: newPeg -> neighbors -> check already connected (opponents) function -> updatedNeighbors
dropSpoilPegs newPeg neighborPegs alreadyConnected = 
    let neighborPegsPos = map pegPos neighborPegs
        newPegPos = pegPos newPeg
        posPairs = zip neighborPegsPos $ replicate (length neighborPegsPos) newPegPos
        --new peg + some neighbor -> bridges they would spoil if connected
        spoilPairs = map genSpoilPairs posPairs
        --now check which of these pairs are actually already connected
        spoiled = map (any id . (map $ uncurry alreadyConnected)) spoilPairs 
    --keep only those neighbors which are not spoiled
    in map fst $ filter (not . snd) $ zip neighborPegs spoiled

genGoodNeighbors :: Board -> Peg -> Pegs -> Pegs
genGoodNeighbors board newPeg neighbors = 
    let isBridge = \pos1 pos2 -> arePosConnected board [pos1, pos2]
    in dropSpoilPegs newPeg neighbors isBridge  

-- ==============================
--connecting pegs and merging groups
-- ==============================

relJumps :: [Pos]
relJumps = [(1, -2), (1, 2), (2, 1), (2, -1), (-1, 2), (-1, -2), (-2, 1), (-2, -1)]

jumps :: Pos -> [Pos]
jumps (posy, posx) = map (\(rely, relx) -> (posy + rely, posx + relx)) relJumps

getConnectedPegs :: Peg -> Pegs -> Pegs
getConnectedPegs peg pegs = let connectedPos = jumps (pegPos peg)
                            in filter (\p -> pegColor(p) == pegColor(peg) && elem (pegPos p) connectedPos) pegs

mergeGroups :: [Group] -> Group -> Group
mergeGroups [] group = let nubPegs = nub $ grpPegs group
                       in Group{ grpColor = grpColor(group),
                                 grpPegs = nubPegs
                               }
mergeGroups (g:gs) group = let updatedGroup = Group { grpColor = grpColor(group),
                                                    grpPegs = grpPegs(group) ++ grpPegs(group)
                                                  }
                         in mergeGroups gs group

arePosConnected :: Board -> [Pos] -> Bool
arePosConnected board positions = let groups = map (\pos -> Map.lookup (pos) (bdGroups board)) positions
                                      nubbed = nub groups
                                  in length nubbed == 1 && nubbed !! 0 /= Nothing

arePegsConnected :: Board -> Pegs -> Bool
arePegsConnected board pegs = arePosConnected board $ map pegPos pegs 

-- ==============================
--move placing and wrappers
-- ==============================

-- expects legal move
placePeg :: Board -> Peg -> Board
placePeg board peg | not $ isLegalMove board peg = error "illegal move"
placePeg board peg | otherwise =
    let newPegs = peg:(bdPegs board)
        -- find neighboring pegs
        neighbors = getConnectedPegs peg (bdPegs board)
        -- consider only those we can connectTo
        goodNeighbors = genGoodNeighbors board peg neighbors 
        -- resolve neighboring pegs to groups TODO lookup might return Maybe
        neighborGroups = exportMaybes $
                         map (\peg -> Map.lookup (pegPos peg) (bdGroups board)) goodNeighbors
        -- connect all the neighboring groups
        newGroup = mergeGroups neighborGroups (mkGroup peg)
        -- update groups
        newGroups = foldl (\groups pos -> Map.update (\group->Just newGroup) pos groups)
                        (Map.insert (pegPos peg) newGroup (bdGroups board)) $
                        map (\peg -> pegPos peg) $ goodNeighbors ++ [peg]
        --newGroups = Map.fromList [((pegPos peg), newGroup)]
     -- create the board
     in Board {bdSize = bdSize board,
               bdPegs = newPegs,
               bdGroups = newGroups,
               bdToPlay = bdToPlay board
              }

--silently falls back to original board if move not legal
placePegFallback :: Board -> Peg -> Board
placePegFallback board peg | not $ isLegalMove board peg = board
placePegFallback board peg | otherwise = placePeg board peg

--if not legal move returns Nothing
placePegMaybe :: Board -> Peg -> Maybe Board
placePegMaybe board peg | not $ isLegalMove board peg = Nothing
placePegMaybe board peg | otherwise = Just $ placePeg board peg

--places a sequence of pegs - expects move to be legal
placePegSeq :: Board -> Pegs -> Board
placePegSeq initBoard [] = initBoard
placePegSeq initBoard seq = foldl placePeg initBoard seq

-- ==============================
--winner recognition functionality
-- ==============================

pegOnRow :: Peg -> Int -> Bool
pegOnRow peg row = (getRow $ pegPos peg) == row

pegOnCol :: Peg -> Int -> Bool
pegOnCol peg col = (getCol $ pegPos peg) == col

isWinningGroup :: Board -> Group -> Bool
isWinningGroup board group | grpColor group == White 
    = any (flip pegOnRow 0) (grpPegs group) && 
      any (flip pegOnRow (getRow (bdSize board) - 1)) (grpPegs group) 
isWinningGroup board group | grpColor group == Black 
    = any (flip pegOnCol 0) (grpPegs group) && 
      any (flip pegOnCol (getCol (bdSize board) - 1)) (grpPegs group) 

getWinner :: Board -> Maybe Color
getWinner board = let winningGroups = filter (isWinningGroup board) $ nub $ Map.elems $ bdGroups board
                  in getWinnerFromGroups winningGroups

getWinnerFromGroups :: [Group] -> Maybe Color 
getWinnerFromGroups [] = Nothing
getWinnerFromGroups (group:[]) = Just $ grpColor group
getWinnerFromGroups _ = error "More than one winning group"

