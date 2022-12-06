module Ptest (module Ptest) where

import Test.QuickCheck
import qualified Lib as L

genSlot :: Gen [L.Slot]
genSlot = do
            a <- chooseInt (0,5)
            b <- chooseInt (0,5)
            c <- chooseInt (0,5)
            d <- chooseInt (0,5)
            return (L.intsToSlots [a,b,c,d])

-- generate random pair of slots, duplicate combination possible
genSlotTest :: Gen ([L.Slot],[L.Slot])
genSlotTest = do
                x <- genSlot
                y <- genSlot
                return (x,y)

genUniqueSlot :: Gen [L.Slot]
genUniqueSlot = do
                a <- chooseInt (0,5)
                b <- chooseInt (0,5)
                c <- chooseInt (0,5)
                d <- chooseInt (0,5)
                if L.hasNoDuplicate [a,b,c,d]
                    then return (L.intsToSlots [a,b,c,d])
                    else genUniqueSlot 

-- generate random pair of slots, duplicate combination impossible
genUniqueTest :: Gen ([L.Slot],[L.Slot])
genUniqueTest = do
                x <- genUniqueSlot
                y <- genUniqueSlot
                return (x,y)

genDkTest :: Gen ([[L.Slot]], [L.Slot], [L.Slot])
genDkTest = do
                x <- genUniqueSlot
                y <- genUniqueSlot
                return (L.searchS,x,y)


invalidJudge :: ([L.Slot],[L.Slot]) -> Bool
invalidJudge (test, guess) = res /= invalid
    where
        res = L.masterJudge test guess
        invalid = [L.red, L.red, L.red, L.white]

emptyJudge :: ([L.Slot],[L.Slot]) -> Bool
emptyJudge (test, guess) = res /= L.emp
    where
        res = L.masterJudge test guess

empty1Judge :: ([L.Slot],[L.Slot]) -> Bool
empty1Judge (test, guess) = res /= invalid
    where
        res = L.masterJudge test guess
        invalid = [L.white, L.Empty, L.Empty, L.Empty]

redJudge :: ([L.Slot],[L.Slot]) -> Bool
redJudge x@(test, guess) = numRed == numRedRes
    where
        res       = L.masterJudge test guess
        numRedRes = length (filter (== L.red) res)
        numRed    = compareM x

compareM :: ([L.Slot],[L.Slot]) -> Int
compareM ([],[]) = 0
compareM (x:xs, y:ys) = if x == y then 1 + compareM (xs, ys) else compareM (xs, ys)

judgeSize :: ([L.Slot],[L.Slot]) -> Bool
judgeSize (test, guess) = resSize == 4
    where
        res     = L.masterJudge test guess
        resSize = length res

aiSpaceReduce :: ([[L.Slot]], [L.Slot], [L.Slot]) -> Bool
aiSpaceReduce (space, test, guess) = ogSize > newSize 
    where
        ogSize   = length space
        newSpace = L.dkSearch space test guess
        newSize  = length newSpace


-- No judgement should return [R,R,R,W] as a result
prop_genSlotInvalid :: Property
prop_genSlotInvalid = forAll genSlotTest invalidJudge

-- Tests without duplciates, no judement should return [] as a result
-- Minimum [W,W] should be returned
prop_genSlotEmpty :: Property
prop_genSlotEmpty = forAll genUniqueTest emptyJudge

-- No judgement should return [W]
-- Minimum [W,W] should be returned
prop_genSlotEmpty1 :: Property
prop_genSlotEmpty1 = forAll genUniqueTest empty1Judge

-- number of red pins should match number of color correct pegs placed at correct positions
prop_redJudge :: Property
prop_redJudge = forAll genSlotTest redJudge

-- All judgement should be size 4
prop_judgeSize :: Property
prop_judgeSize = forAll genSlotTest judgeSize

prop_dkSearchSize :: Property
prop_dkSearchSize = forAll genDkTest aiSpaceReduce

-- Check for 10000 tests
quickCheckN :: Testable prop => Int -> prop -> IO ()
quickCheckN n = quickCheckWith (stdArgs {maxSuccess = n})


-- Results:
-- *Main Lib LibTest Paths_MM Ptest Tui Test.QuickCheck> quickCheckN 10000 prop_genSlotInvalid 
-- +++ OK, passed 10000 tests.

-- *Main Lib LibTest Paths_MM Ptest Tui Test.QuickCheck> quickCheckN 10000 prop_genSlotEmpty 
-- +++ OK, passed 10000 tests.

-- *Main Lib LibTest Paths_MM Ptest Tui Test.QuickCheck> quickCheckN 10000 prop_genSlotEmpty1 
-- +++ OK, passed 10000 tests.

-- *Main Lib LibTest Paths_MM Ptest Tui Test.QuickCheck> quickCheckN 10000 prop_genredJudge
-- +++ OK, passed 10000 tests.

-- *Main Lib LibTest Paths_MM Ptest Tui Test.QuickCheck> quickCheckN 10000 prop_dkSearchSize
-- +++ OK, passed 10000 tests.

-- *Main Lib LibTest Paths_MM Ptest Tui Test.QuickCheck> quickCheckN 10000 prop_judgeSize
-- +++ OK, passed 10000 tests.