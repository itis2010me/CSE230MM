module LibTest (module LibTest) where

import Lib

get1st (a,_,_) = a
get2nd (_,a,_) = a
get3rd (_,_,a) = a

-- assert :: Bool -> String -> String
-- assert False _ = error "Error! Failed Test"
-- assert _ x = x

checkTestCase :: [([Slot], [Slot], [Slot])] -> [Bool]
checkTestCase xs = map checkSingleTestCase xs

checkSingleTestCase :: ([Slot], [Slot], [Slot]) -> Bool
checkSingleTestCase testCase = (masterJudge s g == ans)
    where
        s = get1st testCase
        g = get2nd testCase
        ans = get3rd testCase

testCase :: [([Slot], [Slot], [Slot])]
testCase = [
    ([red, red, red, red],[red, red, red, red],[red, red, red, red])
            ]

libTest :: IO ()
libTest = do 
    putStrLn (show (map show (checkTestCase testCase)))