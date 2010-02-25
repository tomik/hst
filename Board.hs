module Board where

import qualified Data.Map as Map
import Data.List

import Utils (slice, mapFetch)

-- ==============================
-- declarations and instancing
-- ==============================

--white connects top to bottom
--black connects left to right
data Color = Black | White deriving Eq

instance Show Color where
    show Black = "B"
    show White = "W"

type Coord = Int

--size is (maxrow + 1, maxcol + 1)
type Size = (Coord, Coord)
--pos is (row, col) where row|col is in [0, maxrow|maxcol -1]
type Pos = (Coord, Coord)

type GroupId = Pos

data Peg = Peg {pegPos :: Pos, pegColor :: Color} deriving (Eq)
type Pegs = [Peg]
type PegMap = Map.Map Pos Peg

data Group = Group {grpColor :: Color, grpId :: GroupId, 
                    grpMinCoord :: Coord, grpMaxCoord :: Coord} deriving (Show, Eq)
type GroupMap = Map.Map GroupId Group

data Board = Board {bdSize :: Size, 
                    bdPegMap:: PegMap, 
                    bdToPlay :: Color, 
                    bdGroupMap :: GroupMap, 
                    bdWinner :: Maybe Color
                    } deriving (Eq)

-- ==============================
-- representation 
-- ==============================

colorToJsonStr :: Color -> String
colorToJsonStr White = "1"
colorToJsonStr Black = "2"

pegToJsonStr :: Peg -> String
pegToJsonStr peg = "{\"player\": " ++ (colorToJsonStr (pegColor peg)) ++ ", \"x\":" ++ 
                   (show $ getCol(pegPos peg) + 1) ++ ", \"y\":" ++ (show $ getRow(pegPos peg) + 1) ++
                   ", \"type\": 1}"

pegsToJsonStr :: Pegs -> String
pegsToJsonStr pegs = "[" ++ (intercalate "," (map pegToJsonStr pegs))  ++ "]" 
showSquare :: Board -> Pos -> String
showSquare board pos = let value = Map.lookup pos (bdPegMap board)
                       in case value of
                       Nothing -> " . "
                       Just peg -> " " ++ show (pegColor peg) ++ " "

instance Show Peg where 
    show peg = "[" ++ show (pegPos peg) ++ show (pegColor peg) ++ "]"

instance Show Board where
    show board = 
        let (sizey, sizex) = bdSize board
            line = [ showSquare board (row, col) | row <- [0, 1..(sizey - 1)], col <- [0, 1..(sizex - 1)] ]
            header = "Board " ++ (show $ bdToPlay board) ++ " to play:"
        in unlines $ [header] ++
                     (map concat $ slice sizex line)
                     -- ++ ["Groups: " ++ show (Map.elems $ bdGroupMap board)]
    
-- ==============================
-- constructors and convenience functions
-- ==============================

mkBoard :: Coord -> Coord -> Board
mkBoard sizey sizex = 
    let size = (sizey, sizex) in 
                    Board {
                        bdSize = size,
                        bdToPlay = White,
                        bdPegMap = Map.empty,
                        bdGroupMap = Map.empty,
                        bdWinner = Nothing
                        }

mkGroup :: Peg -> Group
mkGroup peg = 
    let coord = if pegColor peg == White 
                then getRow (pegPos peg) 
                else getCol (pegPos peg)
    in Group{
        grpColor=pegColor peg,
        grpId=pegPos peg,
        grpMinCoord=coord,
        grpMaxCoord=coord
        }

mkPeg :: Coord -> Coord -> Color -> Peg
mkPeg posy posx color = Peg{pegPos=(posy, posx), pegColor=color}

--abs + rel pos -> pos
offsetPos :: Pos -> Pos -> Pos
offsetPos (posy, posx) (offy, offx) = (posy + offy, posx + offx)

oppColor :: Color -> Color
oppColor White = Black
oppColor Black = White

getRow :: Pos -> Coord
getRow (row, col) = row

getCol :: Pos -> Coord
getCol (row, col) = col

pegOnRow :: Peg -> Coord -> Bool
pegOnRow peg row = (getRow $ pegPos peg) == row

pegOnCol :: Peg -> Coord -> Bool
pegOnCol peg col = (getCol $ pegPos peg) == col

mkPegs :: [(Coord, Coord, Color)] -> Pegs
mkPegs [] = []
mkPegs ((posy, posx, color):rest) = (mkPeg posy posx color):(mkPegs rest)

pegsByColor :: Pegs -> Color -> Pegs
pegsByColor pegs color = filter (\peg -> pegColor peg == color) pegs

whitePegs = flip pegsByColor White
blackPegs = flip pegsByColor Black

pegSameColor :: Peg -> Peg -> Bool
pegSameColor peg1 peg2 = pegColor peg1 == pegColor peg2

getBoardPegs :: Board -> Pegs
getBoardPegs board = Map.elems $ bdPegMap board

-- ==============================
-- playable positions
-- ==============================

getCorners :: Size -> [Pos] 
getCorners size = 
    [(0, 0), 
     (getRow size - 1, 0), 
     (getRow size - 1, getCol size - 1), 
     (0, getCol size - 1)]

getSpecialPos :: Size -> Color -> [Pos] 
getSpecialPos size White = 
    [(y, x) | y <- [0, 1..(getRow size - 1)], x <- [1.. getCol size - 2]]
getSpecialPos size Black = 
    [(y, x) | x <- [0, 1..(getCol size - 1)], y <- [1.. getRow size - 2]]

getCommonPos :: Size -> [Pos]
getCommonPos size = 
              [(y, x) | y <- [1..(getRow size - 2)], 
                        x <- [1..(getCol size - 2)]]
              \\ (getCorners size)

getAllPlayablePos :: Board -> [Pos] 
getAllPlayablePos board = 
    let size = bdSize board in 
    nub $ (getSpecialPos size White) ++ (getSpecialPos size Black)
    ++ (getCommonPos size)

getPlayablePos :: Board -> Color -> [Pos] 
getPlayablePos board color = 
    let all = (getSpecialPos (bdSize board) color) ++ 
              (getCommonPos (bdSize board)) 
    in (nub all \\ (Map.keys $ bdPegMap board)) 

-- ==============================
-- limitations
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

isEmptySquare :: Board -> Pos -> Bool 
isEmptySquare board pos =  Map.lookup pos (bdPegMap board) == Nothing 

--on the board, not on the opponents edge and on the empty square
isLegalMove :: Board -> Peg -> Bool
isLegalMove board peg = isOnBoard board peg
                        && not (isGoalEdge (bdSize board) (pegPos peg) (oppColor $ pegColor peg))
                        && Map.lookup (pegPos peg) (bdPegMap board) == Nothing 

-- ==============================
-- bridge spoiling
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
doGenSpoilPairs processor (pos1@(pos1y, pos1x), pos2@(pos2y, pos2x)) =
    let processed = map (\(off1, off2) -> (processor off1, processor off2)) relSpoilPairs
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

--drops neighbors new peg cannot connect to 
genGoodNeighbors :: Board -> Peg -> Pegs -> Pegs
genGoodNeighbors board newPeg neighbors = dropSpoilPegs newPeg neighbors (isBridge board)

-- ==============================
-- connecting pegs and merging groups
-- ==============================

relJumps :: [Pos]
relJumps = [(1, -2), (1, 2), (2, 1), (2, -1), (-1, 2), (-1, -2), (-2, 1), (-2, -1)]

jumps :: Pos -> [Pos]
jumps (posy, posx) = map (\(rely, relx) -> (posy + rely, posx + relx)) relJumps

filterConnectedPegs :: Peg -> Pegs -> Pegs
filterConnectedPegs peg toFilter =  
    let connectedPos = jumps (pegPos peg)
    in filter (\apeg -> pegSameColor peg apeg && elem (pegPos apeg) connectedPos) toFilter

getConnectedPegs :: Board -> Peg -> Pegs
getConnectedPegs board peg = 
    let connectedPos = jumps (pegPos peg)
    in filter (\apeg -> pegSameColor peg apeg) $ mapFetch connectedPos (bdPegMap board)

--filterConnectedPegs peg $ getBoardPegs board

mergeGroups :: [Group] -> Group -> Group
mergeGroups [] group = group
mergeGroups (g:gs) group = 
    let updatedGroup = Group {grpColor = grpColor group, 
                              grpId = grpId group, 
                              grpMinCoord = min (grpMinCoord group) (grpMinCoord g),
                              grpMaxCoord = max (grpMaxCoord group) (grpMaxCoord g)}
    in mergeGroups gs updatedGroup

--checks whether all the pegs have same group number
arePosConnected :: Board -> [Pos] -> Bool
arePosConnected board positions = let nubbed = nub $ mapFetch positions (bdGroupMap board) 
                                  in length nubbed <= 1 

isBridge :: Board -> Pos -> Pos -> Bool
isBridge board pos1 pos2 = 
    let group1 = Map.lookup pos1 (bdGroupMap board)
        group2 = Map.lookup pos2 (bdGroupMap board)
    in group1 == group2 && group1 /= Nothing

arePegsConnected :: Board -> Pegs -> Bool
arePegsConnected board pegs = arePosConnected board $ map pegPos pegs 

-- ==============================
-- move placing and wrappers
-- ==============================

--TODO optimize 
getPosForGroup :: Board -> Group -> [Pos]
getPosForGroup board group = Map.keys $ Map.filter ((==) group) (bdGroupMap board) 

-- expects legal move
placePeg :: Board -> Peg -> Board
placePeg board peg | not $ isLegalMove board peg = error "illegal move"
placePeg board peg | otherwise =
    let -- pegMapcontaining new peg
        newPegMap = Map.insert (pegPos peg) peg (bdPegMap board)
        -- update groupMap with newgroups id
        neighbors = getConnectedPegs board peg 
        -- consider only those we can connectTo
        goodNeighbors = genGoodNeighbors board peg neighbors 
        -- connect all the neighboring groups
        groupsToUpdate = nub $ mapFetch (map pegPos goodNeighbors) (bdGroupMap board)
        -- collect all the positions that must be updated
        posToUpdate = concat $ map (getPosForGroup board) groupsToUpdate
        -- merge groups in groupMap
        newGroup = mergeGroups groupsToUpdate $ mkGroup peg
        -- find neighboring pegs
        newGroupMap = foldl (\groupMap pos -> Map.insert pos newGroup groupMap) 
                      (bdGroupMap board) $ posToUpdate ++ [pegPos peg]

     -- create the board
     in Board {bdSize = bdSize board,
               bdPegMap = newPegMap,
               bdGroupMap = newGroupMap,
               bdToPlay = oppColor $ bdToPlay board,
               bdWinner = getWinnerFromGroup (bdSize board) newGroup (bdWinner board)
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
-- winner recognition functionality
-- ==============================

getWinnerFromGroup :: Size -> Group -> Maybe Color -> Maybe Color
getWinnerFromGroup size group oldWinner | oldWinner /= Nothing = oldWinner
getWinnerFromGroup size group oldWinner | isWinGroup size group = Just (grpColor group)
getWinnerFromGroup size group oldWinner = Nothing

isWinGroup :: Size -> Group -> Bool
isWinGroup size group | grpColor group == White 
    = grpMinCoord group == 0 && grpMaxCoord group == getRow size - 1
isWinGroup size group | grpColor group == Black 
    = grpMinCoord group == 0 && grpMaxCoord group == getCol size - 1

getWinner :: Board -> Maybe Color
getWinner board = bdWinner board

hasWinner :: Board -> Bool
hasWinner board = getWinner board /= Nothing

isDraw :: Board -> Bool
isDraw board = 
    let (y, x) =  bdSize board
    in Map.size (bdPegMap board) == x * y - 4
