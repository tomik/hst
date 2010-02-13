import Test.HUnit
import Board

-- ==============================
-- Board testing
-- ==============================


sequenceAssertions assertions = sequence assertions >> return ()

refSizex = 6
refSizey = 6
refSize = (refSizey, refSizex)
refBoard = mkBoard refSizex refSizey
refPos1 = (1, 1)

--peg connecting
-- ==============================
{-
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

testPegConnect = TestCase $ assertEqual "testPegConnect" connectedPegs $
                 getConnectedPegs (mkPeg 3 3 White) (connectedPegs ++ separatePegs)

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

--bridge spoiling
-- ==============================

--peg placing 
-- ==============================

--winning check
-- ==============================

boardTest = TestList [testPegConnect, testInvalidPegs, testInvalidPegSeqs]
runTests = runTestTT boardTest
run = runTests

main = runTests

