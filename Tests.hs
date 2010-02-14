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

--peg validity 
-- ==============================

--peg cannot be placed
invalidPegs = mkPegs [
              --corners
              (0, 0, White), (refSizex - 1, refSizey - 1, White), 
              (refSizex - 1, 0, White), (0, refSizey - 1, White),
              --only white
              (0, 1, Black), (refSizex - 1, 1, Black),
              --out ot the board
              (-1, 0, White), (refSizex, 0, White)
              ]

testInvalidPegs = let newTestBoards = map (placePegFallback refBoard) invalidPegs
                  in TestCase $ sequenceAssertions$ map (assertEqual "testInvalidPegs fail" refBoard) newTestBoards

--last of the sequence should not be placed
invalidPegSeqs = [
              --two pegs at the same position same colors
              ([Peg{pegPos=refPos1, pegColor=White}], Peg{pegPos=refPos1, pegColor=White}),
              --two pegs at the same position different colors
              ([Peg{pegPos=refPos1, pegColor=White}], Peg{pegPos=refPos1, pegColor=Black})
                 ]

testInvalidPegSeqs = let testBoards = map (\(seq, peg) -> placePegSeq refBoard seq) invalidPegSeqs
                         (_, invalidPegs) = unzip invalidPegSeqs
                         newTestBoards = map (\(board, peg) -> placePegFallback board peg) (zip testBoards invalidPegs)
                     in TestCase $
                        sequenceAssertions $
                        map (\(b1, b2) -> assertEqual "testInvalidPegSeqs fail" b1 b2) (zip testBoards newTestBoards)

--peg connecting
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


--peg spoiling pair generation
-- ==============================

--low level spoiling functionality
--
{- scenario 1(b), 2(w)
 .  w  .  .  .  .  .
 .  .  .  b  .  .  .
 .  b  w  .  .  .  .
-}
{- scenario 3, 4
 .  w  .  b  .  .  .
 .  .  .  w  .  .  .
 .  .  b  .  .  .  .
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

--peg bridges 
-- ==============================

{- scenario:
 .  w  .  .  .  .  .
 .  .  . b1  .  .  .
 . b1  w  .  .  .  .
 .  .  .  .  .  .  .
 .  .  .  .  .  .  .
 .  .  .  .  .  .  .
 -}

--in jest data white is always connected with all pegs whilst black is not connected with a;;
dataBridges = [mkPegs [(2, 1, Black), (0, 1, White), (1, 3, Black), (2, 2, White)]]

bridgeCheck :: Board -> Color -> Bool
bridgeCheck board color = arePegsConnected board (pegsByColor (getBoardPegs board) color)

mkTestBridge :: Pegs -> Test
mkTestBridge pegSeq = 
                  let testBoard = placePegSeq refBoard pegSeq
                      resultSpoil = not . (flip bridgeCheck Black) $ testBoard
                      resultBuild = flip bridgeCheck White $ testBoard
                  in TestCase $ sequenceAssertions $
                     [putStrLn (show testBoard),
                      assertBool "testBridgeBuild fail" resultBuild,
                      assertBool "testBridgeSpoil fail" resultSpoil]


testBridges = TestList $ map mkTestBridge dataBridges

--peg placing 
-- ==============================

--winning check
-- ==============================

-- ==============================
-- Test running
-- ==============================

boardTest = TestList [testPegConnect, testInvalidPegs, testInvalidPegSeqs, 
                      testGenSpoilPairs, testBridges]
runTests = runTestTT boardTest
run = runTests

main = runTests

