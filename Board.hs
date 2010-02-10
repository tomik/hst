import qualified Data.Map as Map
import Data.List 

import Utils (slice)

data Color = Black | White deriving Eq

instance Show Color where
    show Black = "B"
    show White = "W"

type Pos = (Int, Int)
type Size = (Int, Int)

data Peg = Peg {pegPos :: Pos, pegColor :: Color} deriving (Show, Eq)
type Pegs = [Peg] 

data Group = Group {grpColor:: Color, grpPegs :: [Peg]} deriving Show
type Groups = Map.Map Pos Group

data Board = Board {bdSize :: Size, bdPegs:: Pegs, bdToPlay :: Color, bdGroups :: Groups} 

showSquare :: Board -> Pos -> String
showSquare board pos = let value = Map.lookup pos (bdGroups board)
                       in case value of 
                       Nothing -> " . "
                       Just group -> " " ++ show (grpColor group) ++ " "

instance Show Board where 
    show board = let (sizex, sizey) = bdSize board
                     line = [ showSquare board (row, col) | row <- [1, 2..sizex], col <- [1, 2..sizey] ]
                 in unlines $ (map concat $ slice sizex line) ++ [show (bdPegs board)] ++ [show (bdGroups board)]

    --show board = foldl (++) "" [show $ pegPos peg | peg <- bdPegs board]

oppColor :: Color -> Color
oppColor White = Black
oppColor Black = White

getRow :: Pos -> Int 
getRow (row, col) = row 

getCol :: Pos -> Int 
getCol (row, col) = col

mkBoard :: Int -> Int -> Board
mkBoard sizex sizey = Board {
                        bdSize = (sizex, sizey), 
                        bdPegs = [],
                        bdToPlay = White,
                        bdGroups = Map.empty
                        }

mkGroup :: Peg -> Group
mkGroup peg = Group{
                    grpColor=pegColor peg,
                    grpPegs=[peg]
                    }

relJumps :: [Pos]
relJumps = [(1, -2), (1, 2), (2, 1), (2, -1), (-1, 2), (-1, -2), (-2, 1), (-2, -1)]

jumps :: Pos -> [Pos]
jumps (posx, posy) = map (\(relx, rely) -> (posx + relx, posy + rely)) relJumps

getConnectedPegs :: Peg -> Pegs -> Pegs
getConnectedPegs peg pegs = let connectedPos = jumps (pegPos peg) 
                            in filter (\p -> pegColor(p) == pegColor(peg) && elem (pegPos p) connectedPos) pegs

isGoalEdge:: Size -> Pos -> Color -> Bool
isGoalEdge (0, _) _ White = True
isGoalEdge (_, 0) _ Black = True
isGoalEdge (row1, _) (row2, _) White | row1 == row2 = True
isGoalEdge (_, col1) (_, col2) Black | col1 == col2 = True
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
                        && not (isGoalEdge (bdSize board) (pegPos peg) (pegColor peg))
                        && length (filter (\p -> (pegPos peg) == (pegPos p)) (bdPegs board)) == 0

mergeGroups :: [Group] -> Group -> Group
mergeGroups [] group = let nubPegs = nub $ grpPegs group 
                       in Group{ grpColor = grpColor(group), 
                                 grpPegs = nubPegs
                               }
mergeGroups (g:gs) group = let updatedGroup = Group { grpColor = grpColor(group), 
                                                    grpPegs = grpPegs(group) ++ grpPegs(group)
                                                  }
                         in mergeGroups gs group

exportMaybes :: [Maybe a] -> [a]
exportMaybes [] = [] 
exportMaybes (Just x:xs) = x:(exportMaybes xs)
exportMaybes (Nothing:xs) = exportMaybes xs


placePeg :: Board -> Peg -> Board
placePeg board peg = let newPegs = peg:(bdPegs board)
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

--TODO

isWinningGroup :: Board -> Group -> Bool
isWinningGroup board group = False

getWinner :: Board -> Maybe Color
getWinner board = Nothing

--testing 

empty1 = placePeg (mkBoard 3 4) Peg{pegPos=(1, 1), pegColor=White}

nlPrint :: String -> IO ()
nlPrint s = putStrLn s

