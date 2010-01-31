import Data.Map as Map
import Utils (slice)

data Color = Black | White

instance Show Color where
    show Black = "B"
    show White = "W"

type Pos = (Int, Int)
type Size = (Int, Int)

data Peg = Peg {pegPos :: Pos, pegColor :: Color} deriving Show
type Pegs = [Peg] 

data Group = Group {grpColor:: Color, grpPegs = [Peg]} deriving Show
type Groups = Map Pos Group

data Board = Board {bdSize :: Size, bdPegs:: Pegs, bdToPlay :: Color, bdGroups :: Group} 

--TODO somehow push this to instance Show ?
showSquare Nothing = " x "
showSquare (Just peg) = " " ++ show (pegColor peg) ++ " "

instance Show Board where 
    show board = unlines $ map concat $ slice sizex line 
        --all the board in one line
        where line  = [showSquare square | square <- elems $ bdSquares board]
              sizex = fst $ bdSize board

oppColor :: Color -> Color
oppColor White = Black
oppColor Black = White

getRow :: Pos -> Int 
getRow (row, col) = row 

getCol :: Pos -> Col 
getCol (row, col) = col

mkBoard :: Int -> Int -> Board
mkBoard sizex sizey = Board {
                        bdSize = (sizex, sizey), 
                        bdPegs = [],
                        bdToPlay = White
                        bdGroups = Map.empty
                        }

mkPeg :: Pos -> Color -> Peg
mkPeg pos color = Peg { 
                    pegPos = pos, 
                    pegColor = color
                    }

relJumps :: [Pos]
relJumps = [(1, -2), (1, 2), (2, 1), (2, -1), (-1, 2), (-1, -2), (-2, 1), (-2, -1)]

jumps :: Pos -> [Pos]
jumps (posx, posy) = map (\relx rely-> (posx + relx, posy + rely)) relJumps

isGoalEdge:: Size -> Pos -> Color -> Bool
isGoalEdge (0, _) _ White = True
isGoalEdge (_, 0) _ Black = True
isGoalEdge (row, _) (row, _) White = True
isGoalEdge (_, col) (_, col) Black = True
isGoalEdge _ _ _ = False

getConnected :: Peg -> Pegs -> Pegs
--getConnected 

isOnBoard :: Board -> Peg -> Bool
--isOnBoard board peg = getRow(pegPos(peg)) <= get(RowbdSize

isEdge :: Pos -> Size -> Bool
isEdge pos size = (isGoalEdge pos size White) || (isGoalEdge pos size Black)

--not on the opponents edge and on the empty square
isLegalMove :: Board -> Peg -> Bool
isLegalMove board peg = not $ isGoalEdge bdSize(board) pegPos(peg) pegColor(peg) 
                        and length (filter (\p -> pegPos(peg) == pegPos(p)) bdPegs(board)) == 0

placePeg :: Board -> Peg -> Board
placePeg board peg = let newPegs = peg:Pegs
                         --newGroups = insert pegPos(peg) Peg 
                     in Board bdSize(board) newPegs bdToPlay(board)

isWinningGroup :: Board -> Group -> Bool

getWinner :: Board -> Maybe Color

--testing stuff

empty1 = mkBoard 3 4

nlPrint :: String -> IO ()
nlPrint s = putStrLn s

