import Test.HUnit
import Data.List
import Utils
import Board

sequenceAssertions assertions = sequence assertions >> return ()

-- ==============================
-- Board testing
-- ==============================

refSizex = 6
refSizey = 6
refSize = (refSizey, refSizex)
refBoard = mkBoard refSizex refSizey
refPos1 = (1, 1)

-- peg validity 
-- ==============================

outOfBoardPegs = mkPegs [
    --corners
    (0, 0, White), 
    (0, refSizey - 1, White),
    (refSizey - 1, refSizex - 1, White), 
    (refSizey - 1, 0, White), 
    --only white can play
    (0, 1, Black), 
    (refSizey - 1, 1, Black),
    --only black can play
    (1, 0, White), 
    (1, refSizex -1, White),
    --out ot the board
    (-1, 0, White), (refSizey, 0, White)
    ]

--peg cannot be placed
testOutOfBoard = let newTestBoards = map (placePegFallback refBoard) outOfBoardPegs
                 in TestCase $ sequenceAssertions$ map (assertEqual "testOutOfBoard fail" refBoard) newTestBoards

--tests that it is not possible to place peg on peg 
testPegOnPeg = 
    TestCase $ do
    let b1 = placePegFallback refBoard $ mkPeg 1 1 White 
    let b2 = placePegFallback b1 $ mkPeg 1 1 White 
    assertEqual "testPegOnPeg fail" b1 b2

testInvalidPeg = TestList [testOutOfBoard, testPegOnPeg]

-- peg connecting
-- ==============================

{- scenario:
 - . . . . . . .
 - . . W . W . .
 - . W B . . W .
 - . . . ? B . .
 - . W . W . W .
 - . . B . . . .
 - . . . . . . .
 -}

connectedPegs = mkPegs [(1, 2, White), (1, 4, White), 
                        (2, 1, White), (2, 5, White),
                        (4, 1, White), (4, 5, White)]

separatePegs = mkPegs [(2, 2, Black), (3, 4, Black), 
                       (4, 3, White), (5, 2, Black)]

testPegConnect = TestCase $ assertEqual "testPegConnect fail" connectedPegs $
                 filterConnectedPegs (mkPeg 3 3 White) (connectedPegs ++ separatePegs)

-- peg spoiling pair generation
-- ==============================

--low level spoiling functionality
{-
 scenario 1, 2    scenario 3, 4
 .  w  .  .  .    .  w  .  b  .
 .  .  .  b  .    .  .  .  w  .
 .  b  w  .  .    .  .  b  .  .
-}

--(test number, bridge pair, spoil pair)
dataSpoil = [ ("1", ((2, 1), (1, 3)), ((0, 1), (2, 2))),
              ("2", ((0, 1), (2, 2)), ((2, 1), (1, 3))),
              ("3", ((0, 1), (1, 3)), ((2, 2), (0, 3))),
              ("4", ((2, 2), (0, 3)), ((1, 3), (0, 1)))
            ]
              
--test creator
mkTestSpoil (label, bridgePair, spoiledPair) = TestCase $ 
    do 
    assertBool ("testSpoilSymmetry " ++  label ++ " fail") $ 
        sort (genSpoilPairs bridgePair) == sort (genSpoilPairs $ swapPair bridgePair)
    assertBool ("testSpoilElem " ++ label ++ " fail") $ 
        elem spoiledPair (genSpoilPairs bridgePair) || 
        elem (swapPair spoiledPair) (genSpoilPairs bridgePair)

testGenSpoilPairs = TestList $ map mkTestSpoil dataSpoil

-- peg bridges 
-- ==============================

{-
 scenario1:     scenario2:     
 .  .  .  . .   .  .  .  . .
 .  b  .  . .   .  b w1  . .
 .  .  . w1 .   .  .  .  b .
 . w1  b  . .   . w1  .  . .
 -}

--in test data white is connected while black is not connected (only one bridge possible)
dataBridges = [--scenario1 (in both color combinations)
               mkPegs [(3, 1, White), (2, 3, White), (1, 1, Black), (3, 2, Black)], 
               mkPegs [(1, 1, White), (3, 2, White), (3, 1, Black), (2, 3, Black)], 
               --scenario2 
               mkPegs [(3, 1, White), (1, 2, White), (1, 1, Black), (2, 3, Black)], 
               mkPegs [(1, 1, White), (2, 3, White), (3, 1, Black), (1, 2, Black)]
               ]

bridgeCheck :: Board -> Color -> Bool
bridgeCheck board color = arePegsConnected board (pegsByColor (getBoardPegs board) color)

mkTestBridge :: Pegs -> Test
mkTestBridge pegSeq = 
    let testBoard = placePegSeq refBoard pegSeq
        resultSpoil = not . (flip bridgeCheck Black) $ testBoard
        resultBuild = flip bridgeCheck White $ testBoard
    in TestCase $ sequenceAssertions $
       [assertBool "testBridgeBuild fail" resultBuild,
        assertBool "testBridgeSpoil fail" resultSpoil]

testBridges = TestList $ map mkTestBridge dataBridges

-- peg placing 
-- ==============================

-- winning check
-- ==============================

-- ==============================
-- Test running
-- ==============================

boardTest = TestList [testPegConnect, testInvalidPeg, testGenSpoilPairs, testBridges]
runTests = runTestTT boardTest
run = runTests

main = runTests

