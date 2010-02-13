module Board where

import qualified Data.Map as Map
import Data.List 

import Utils (slice, exportMaybes)

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

data Peg = Peg {pegPos :: Pos, pegColor :: Color} deriving (Show, Eq)
type Pegs = [Peg] 

data Group = Group {grpColor:: Color, grpPegs :: [Peg]} deriving (Show, Eq)
type Groups = Map.Map Pos Group

data Board = Board {bdSize :: Size, bdPegs:: Pegs, bdToPlay :: Color, bdGroups :: Groups} deriving (Eq)

--abs + rel pos -> pos
offsetPos :: Pos -> Pos -> Pos 
offsetPos (posy, posx) (offy, offx) = (posy + offy, posx + offx)

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

    --show board = foldl (++) "" [show $ pegPos peg | peg <- bdPegs board]

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

swapPair :: (a,b) -> (b, a)
swapPair (a, b) = (b, a)

negateFirst :: (Num a) => (a, b) -> (a, b)
negateFirst (a, b) = (-1 * a, b)

--relative positions of cross pairs for bridge at referential position (2`oclock)
relSpoilPairs = [((-2, 0), (0,  1)), ((-1, -1), (0, -1)), ((-1, 0), ( 1,  1)), (( 1, 0), (-1,  1)), 
                 (( 2, 0), (0, -1)), (( 1,  1), (0, -1)), (( 1, 0), (-1, -1)), ((-1, 0), ( 1, -1))]

--for a potential bridge (pair of positions) and processing function gives positions which can spoil it
--processor handles transformations on relSpoilPairs arising from use of non-referential pair orientation
doGenSpoilPairs :: ((Pos, Pos) -> (Pos, Pos)) -> (Pos, Pos) -> [(Pos, Pos)]
doGenSpoilPairs processor (pos1@(pos1x, pos1y), pos2@(pos2x, pos2y))
    = map ((\(off1, off2) -> (offsetPos pos1 off1, offsetPos pos2 off2)) . processor) relSpoilPairs

--wrapper around doGenSpoilPairs
genSpoilPairs :: (Pos, Pos) -> [(Pos, Pos)]
genSpoilPairs posPair@((pos1x, pos1y), (pos2x, pos2y))
    --pos1 should be more on the left
    | pos1x >= pos2x = genSpoilPairs ((pos2x, pos2y), (pos1x, pos1y))
    --1 o`clock
    | pos1y - pos2y == 2 = doGenSpoilPairs (\(pos1, pos2) -> (swapPair pos1, swapPair pos2)) posPair
    --2 o`clock - referential position
    | pos1y - pos2y == 1 = doGenSpoilPairs (\(pos1, pos2) -> (pos1, pos2)) posPair
    --4 o`clock
    | pos2y - pos1y == 1 = doGenSpoilPairs (\(pos1, pos2) -> (negateFirst pos1, pos2)) posPair
    --5 o`clock
    | pos2y - pos1y == 2 = doGenSpoilPairs (\(pos1, pos2) -> (swapPair (negateFirst pos1), swapPair pos2)) posPair
    | otherwise = error "No matching pos pair position"


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

-- expects legal move
placePeg :: Board -> Peg -> Board
placePeg board peg | not $ isLegalMove board peg = error "illegal move"
placePeg board peg | otherwise = 
    let newPegs = peg:(bdPegs board)
        -- find neighboring pegs
        neighbors = getConnectedPegs peg (bdPegs board)
        -- resolve neighboring pegs to groups TODO lookup might return Maybe
        
        neighborGroups = exportMaybes $ 
                         map (\peg -> Map.lookup (pegPos peg) (bdGroups board)) neighbors
        
        -- connect all the neighboring groups
        newGroup = mergeGroups neighborGroups (mkGroup peg)
        -- update groups 
        newGroups = foldl (\groups pos -> Map.update (\group->Just newGroup) pos groups) 
                        (Map.insert (pegPos peg) newGroup (bdGroups board)) $
                        map (\peg -> pegPos peg) $ neighbors ++ [peg]
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


--TODO

isWinningGroup :: Board -> Group -> Bool
isWinningGroup board group = False

getWinner :: Board -> Maybe Color
getWinner board = Nothing

