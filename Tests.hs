import Test.HUnit
import Board

-- ==============================
-- Board testing
-- ==============================


sequenceAssertions assertions = sequence assertions >> return ()

refSizex = 4
refSizey = 4
refSize = (refSizey, refSizex)
refBoard = mkBoard refSizex refSizey
refPos1 = (2, 2)

--bridge spoiling
-- ==============================

--peg connecting
-- ==============================

--peg validity

--peg placing
-- ==============================

--peg cannot be placed
invalidPegs = [
              --corners
              Peg{pegPos=(0, 0), pegColor=White},
              Peg{pegPos=(refSizex - 1, refSizey - 1), pegColor=White},
              Peg{pegPos=(refSizex - 1, 0), pegColor=White},
              Peg{pegPos=(0, refSizey - 1), pegColor=White},
              --only white
              Peg{pegPos=(0, 1), pegColor=Black},
              Peg{pegPos=(refSizex - 1, 1), pegColor=Black},
              --out ot the board
              Peg{pegPos=(-1, 0), pegColor=White},
              Peg{pegPos=(refSizex, 0), pegColor=White}
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

--winning check

boardTest = TestList [testInvalidPegs, testInvalidPegSeqs]
runTests = runTestTT boardTest
run = runTests

main = runTests

