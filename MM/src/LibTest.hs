module LibTest (module LibTest) where

import Lib

get1st :: (a, b, c) -> a
get1st (a,_,_) = a
get2nd :: (a, b, c) -> b
get2nd (_,a,_) = a
get3rd :: (a, b, c) -> c
get3rd (_,_,a) = a

-- assert :: Bool -> String -> String
-- assert False _ = error "Error! Failed Test"
-- assert _ x = x

checkTestCase :: [([Slot], [Slot], [Slot])] -> [Bool]
checkTestCase xs = map checkSingleTestCase xs

checkSingleTestCase :: ([Slot], [Slot], [Slot]) -> Bool
checkSingleTestCase testCases = (masterJudge s g == ans)
    where
        s = get1st testCases
        g = get2nd testCases
        ans = get3rd testCases

countFalseNumber :: [Bool] -> Int
countFalseNumber [] = 0
countFalseNumber (False:xs) = 1 + countFalseNumber xs
countFalseNumber (True:xs) = countFalseNumber xs

testCase :: [([Slot], [Slot], [Slot])]
testCase = [
    ([red, blue, green, white],[red, blue, green, white],[red, red, red, red]),
    ([red, blue, green, white],[red, blue, green, purple],[red, red, red, Empty]),
    ([red, blue, green, white],[red, blue, green, yellow],[red, red, red, Empty]),
    ([red, blue, green, white],[red, blue, white, purple],[red, red, white, Empty]),
    ([red, blue, green, white],[red, blue, white, yellow],[red, red, white, Empty]),
    ([red, blue, green, white],[red, blue, purple, yellow],[red, red, Empty, Empty]),
    ([red, blue, green, white],[red, green, white, purple],[red, white, white, Empty]),
    ([red, blue, green, white],[red, green, white, yellow],[red, white, white, Empty]),
    ([red, blue, green, white],[red, green, purple, yellow],[red, white, Empty, Empty]),
    ([red, blue, green, white],[red, white, purple, yellow],[red, white, Empty, Empty]),
    ([red, blue, green, white],[blue, green, white, purple],[white, white, white, Empty]),
    ([red, blue, green, white],[blue, green, white, yellow],[white, white, white, Empty]),
    ([red, blue, green, white],[blue, green, purple, yellow],[white, white, Empty, Empty]),
    ([red, blue, green, white],[blue, white, purple, yellow],[white, white, Empty, Empty]),
    ([red, blue, green, white],[green, white, purple, yellow],[white, white, Empty, Empty]),
    ([red, blue, green, purple],[red, blue, green, white],[red, red, red, Empty]),
    ([red, blue, green, purple],[red, blue, green, purple],[red, red, red, red]),
    ([red, blue, green, purple],[red, blue, green, yellow],[red, red, red, Empty]),
    ([red, blue, green, purple],[red, blue, white, purple],[red, red, red, Empty]),
    ([red, blue, green, purple],[red, blue, white, yellow],[red, red, Empty, Empty]),
    ([red, blue, green, purple],[red, blue, purple, yellow],[red, red, white, Empty]),
    ([red, blue, green, purple],[red, green, white, purple],[red, red, white, Empty]),
    ([red, blue, green, purple],[red, green, white, yellow],[red, white, Empty, Empty]),
    ([red, blue, green, purple],[red, green, purple, yellow],[red, white, white, Empty]),
    ([red, blue, green, purple],[red, white, purple, yellow],[red, white, Empty, Empty]),
    ([red, blue, green, purple],[blue, green, white, purple],[red, white, white, Empty]),
    ([red, blue, green, purple],[blue, green, white, yellow],[white, white, Empty, Empty]),
    ([red, blue, green, purple],[blue, green, purple, yellow],[white, white, white, Empty]),
    ([red, blue, green, purple],[blue, white, purple, yellow],[white, white, Empty, Empty]),
    ([red, blue, green, purple],[green, white, purple, yellow],[white, white, Empty, Empty]),
    ([red, blue, green, yellow],[red, blue, green, white],[red, red, red, Empty]),
    ([red, blue, green, yellow],[red, blue, green, purple],[red, red, red, Empty]),
    ([red, blue, green, yellow],[red, blue, green, yellow],[red, red, red, red]),
    ([red, blue, green, yellow],[red, blue, white, purple],[red, red, Empty, Empty]),
    ([red, blue, green, yellow],[red, blue, white, yellow],[red, red, red, Empty]),
    ([red, blue, green, yellow],[red, blue, purple, yellow],[red, red, red, Empty]),
    ([red, blue, green, yellow],[red, green, white, purple],[red, white, Empty, Empty]),
    ([red, blue, green, yellow],[red, green, white, yellow],[red, red, white, Empty]),
    ([red, blue, green, yellow],[red, green, purple, yellow],[red, red, white, Empty]),
    ([red, blue, green, yellow],[red, white, purple, yellow],[red, red, Empty, Empty]),
    ([red, blue, green, yellow],[blue, green, white, purple],[white, white, Empty, Empty]),
    ([red, blue, green, yellow],[blue, green, white, yellow],[red, white, white, Empty]),
    ([red, blue, green, yellow],[blue, green, purple, yellow],[red, white, white, Empty]),
    ([red, blue, green, yellow],[blue, white, purple, yellow],[red, white, Empty, Empty]),
    ([red, blue, green, yellow],[green, white, purple, yellow],[red, white, Empty, Empty]),
    ([red, blue, white, purple],[red, blue, green, white],[red, red, white, Empty]),
    ([red, blue, white, purple],[red, blue, green, purple],[red, red, red, Empty]),
    ([red, blue, white, purple],[red, blue, green, yellow],[red, red, Empty, Empty]),
    ([red, blue, white, purple],[red, blue, white, purple],[red, red, red, red]),
    ([red, blue, white, purple],[red, blue, white, yellow],[red, red, red, Empty]),
    ([red, blue, white, purple],[red, blue, purple, yellow],[red, red, white, Empty]),
    ([red, blue, white, purple],[red, green, white, purple],[red, red, red, Empty]),
    ([red, blue, white, purple],[red, green, white, yellow],[red, red, Empty, Empty]),
    ([red, blue, white, purple],[red, green, purple, yellow],[red, white, Empty, Empty]),
    ([red, blue, white, purple],[red, white, purple, yellow],[red, white, white, Empty]),
    ([red, blue, white, purple],[blue, green, white, purple],[red, red, white, Empty]),
    ([red, blue, white, purple],[blue, green, white, yellow],[red, white, Empty, Empty]),
    ([red, blue, white, purple],[blue, green, purple, yellow],[white, white, Empty, Empty]),
    ([red, blue, white, purple],[blue, white, purple, yellow],[white, white, white, Empty]),
    ([red, blue, white, purple],[green, white, purple, yellow],[white, white, Empty, Empty]),
    ([red, blue, white, yellow],[red, blue, green, white],[red, red, white, Empty]),
    ([red, blue, white, yellow],[red, blue, green, purple],[red, red, Empty, Empty]),
    ([red, blue, white, yellow],[red, blue, green, yellow],[red, red, red, Empty]),
    ([red, blue, white, yellow],[red, blue, white, purple],[red, red, red, Empty]),
    ([red, blue, white, yellow],[red, blue, white, yellow],[red, red, red, red]),
    ([red, blue, white, yellow],[red, blue, purple, yellow],[red, red, red, Empty]),
    ([red, blue, white, yellow],[red, green, white, purple],[red, red, Empty, Empty]),
    ([red, blue, white, yellow],[red, green, white, yellow],[red, red, red, Empty]),
    ([red, blue, white, yellow],[red, green, purple, yellow],[red, red, Empty, Empty]),
    ([red, blue, white, yellow],[red, white, purple, yellow],[red, red, white, Empty]),
    ([red, blue, white, yellow],[blue, green, white, purple],[red, white, Empty, Empty]),
    ([red, blue, white, yellow],[blue, green, white, yellow],[red, red, white, Empty]),
    ([red, blue, white, yellow],[blue, green, purple, yellow],[red, white, Empty, Empty]),
    ([red, blue, white, yellow],[blue, white, purple, yellow],[red, white, white, Empty]),
    ([red, blue, white, yellow],[green, white, purple, yellow],[red, white, Empty, Empty]),
    ([red, blue, purple, yellow],[red, blue, green, white],[red, red, Empty, Empty]),
    ([red, blue, purple, yellow],[red, blue, green, purple],[red, red, white, Empty]),
    ([red, blue, purple, yellow],[red, blue, green, yellow],[red, red, red, Empty]),
    ([red, blue, purple, yellow],[red, blue, white, purple],[red, red, white, Empty]),
    ([red, blue, purple, yellow],[red, blue, white, yellow],[red, red, red, Empty]),
    ([red, blue, purple, yellow],[red, blue, purple, yellow],[red, red, red, red]),
    ([red, blue, purple, yellow],[red, green, white, purple],[red, white, Empty, Empty]),
    ([red, blue, purple, yellow],[red, green, white, yellow],[red, red, Empty, Empty]),
    ([red, blue, purple, yellow],[red, green, purple, yellow],[red, red, red, Empty]),
    ([red, blue, purple, yellow],[red, white, purple, yellow],[red, red, red, Empty]),
    ([red, blue, purple, yellow],[blue, green, white, purple],[white, white, Empty, Empty]),
    ([red, blue, purple, yellow],[blue, green, white, yellow],[red, white, Empty, Empty]),
    ([red, blue, purple, yellow],[blue, green, purple, yellow],[red, red, white, Empty]),
    ([red, blue, purple, yellow],[blue, white, purple, yellow],[red, red, white, Empty]),
    ([red, blue, purple, yellow],[green, white, purple, yellow],[red, red, Empty, Empty]),
    ([red, green, white, purple],[red, blue, green, white],[red, white, white, Empty]),
    ([red, green, white, purple],[red, blue, green, purple],[red, red, white, Empty]),
    ([red, green, white, purple],[red, blue, green, yellow],[red, white, Empty, Empty]),
    ([red, green, white, purple],[red, blue, white, purple],[red, red, red, Empty]),
    ([red, green, white, purple],[red, blue, white, yellow],[red, red, Empty, Empty]),
    ([red, green, white, purple],[red, blue, purple, yellow],[red, white, Empty, Empty]),
    ([red, green, white, purple],[red, green, white, purple],[red, red, red, red]),
    ([red, green, white, purple],[red, green, white, yellow],[red, red, red, Empty]),
    ([red, green, white, purple],[red, green, purple, yellow],[red, red, white, Empty]),
    ([red, green, white, purple],[red, white, purple, yellow],[red, white, white, Empty]),
    ([red, green, white, purple],[blue, green, white, purple],[red, red, red, Empty]),
    ([red, green, white, purple],[blue, green, white, yellow],[red, red, Empty, Empty]),
    ([red, green, white, purple],[blue, green, purple, yellow],[red, white, Empty, Empty]),
    ([red, green, white, purple],[blue, white, purple, yellow],[white, white, Empty, Empty]),
    ([red, green, white, purple],[green, white, purple, yellow],[white, white, white, Empty]),
    ([red, green, white, yellow],[red, blue, green, white],[red, white, white, Empty]),
    ([red, green, white, yellow],[red, blue, green, purple],[red, white, Empty, Empty]),
    ([red, green, white, yellow],[red, blue, green, yellow],[red, red, white, Empty]),
    ([red, green, white, yellow],[red, blue, white, purple],[red, red, Empty, Empty]),
    ([red, green, white, yellow],[red, blue, white, yellow],[red, red, red, Empty]),
    ([red, green, white, yellow],[red, blue, purple, yellow],[red, red, Empty, Empty]),
    ([red, green, white, yellow],[red, green, white, purple],[red, red, red, Empty]),
    ([red, green, white, yellow],[red, green, white, yellow],[red, red, red, red]),
    ([red, green, white, yellow],[red, green, purple, yellow],[red, red, red, Empty]),
    ([red, green, white, yellow],[red, white, purple, yellow],[red, red, white, Empty]),
    ([red, green, white, yellow],[blue, green, white, purple],[red, red, Empty, Empty]),
    ([red, green, white, yellow],[blue, green, white, yellow],[red, red, red, Empty]),
    ([red, green, white, yellow],[blue, green, purple, yellow],[red, red, Empty, Empty]),
    ([red, green, white, yellow],[blue, white, purple, yellow],[red, white, Empty, Empty]),
    ([red, green, white, yellow],[green, white, purple, yellow],[red, white, white, Empty]),
    ([red, green, purple, yellow],[red, blue, green, white],[red, white, Empty, Empty]),
    ([red, green, purple, yellow],[red, blue, green, purple],[red, white, white, Empty]),
    ([red, green, purple, yellow],[red, blue, green, yellow],[red, red, white, Empty]),
    ([red, green, purple, yellow],[red, blue, white, purple],[red, white, Empty, Empty]),
    ([red, green, purple, yellow],[red, blue, white, yellow],[red, red, Empty, Empty]),
    ([red, green, purple, yellow],[red, blue, purple, yellow],[red, red, red, Empty]),
    ([red, green, purple, yellow],[red, green, white, purple],[red, red, white, Empty]),
    ([red, green, purple, yellow],[red, green, white, yellow],[red, red, red, Empty]),
    ([red, green, purple, yellow],[red, green, purple, yellow],[red, red, red, red]),
    ([red, green, purple, yellow],[red, white, purple, yellow],[red, red, red, Empty]),
    ([red, green, purple, yellow],[blue, green, white, purple],[red, white, Empty, Empty]),
    ([red, green, purple, yellow],[blue, green, white, yellow],[red, red, Empty, Empty]),
    ([red, green, purple, yellow],[blue, green, purple, yellow],[red, red, red, Empty]),
    ([red, green, purple, yellow],[blue, white, purple, yellow],[red, red, Empty, Empty]),
    ([red, green, purple, yellow],[green, white, purple, yellow],[red, red, white, Empty]),
    ([red, white, purple, yellow],[red, blue, green, white],[red, white, Empty, Empty]),
    ([red, white, purple, yellow],[red, blue, green, purple],[red, white, Empty, Empty]),
    ([red, white, purple, yellow],[red, blue, green, yellow],[red, red, Empty, Empty]),
    ([red, white, purple, yellow],[red, blue, white, purple],[red, white, white, Empty]),
    ([red, white, purple, yellow],[red, blue, white, yellow],[red, red, white, Empty]),
    ([red, white, purple, yellow],[red, blue, purple, yellow],[red, red, red, Empty]),
    ([red, white, purple, yellow],[red, green, white, purple],[red, white, white, Empty]),
    ([red, white, purple, yellow],[red, green, white, yellow],[red, red, white, Empty]),
    ([red, white, purple, yellow],[red, green, purple, yellow],[red, red, red, Empty]),
    ([red, white, purple, yellow],[red, white, purple, yellow],[red, red, red, red]),
    ([red, white, purple, yellow],[blue, green, white, purple],[white, white, Empty, Empty]),
    ([red, white, purple, yellow],[blue, green, white, yellow],[red, white, Empty, Empty]),
    ([red, white, purple, yellow],[blue, green, purple, yellow],[red, red, Empty, Empty]),
    ([red, white, purple, yellow],[blue, white, purple, yellow],[red, red, red, Empty]),
    ([red, white, purple, yellow],[green, white, purple, yellow],[red, red, red, Empty]),
    ([blue, green, white, purple],[red, blue, green, white],[white, white, white, Empty]),
    ([blue, green, white, purple],[red, blue, green, purple],[red, white, white, Empty]),
    ([blue, green, white, purple],[red, blue, green, yellow],[white, white, Empty, Empty]),
    ([blue, green, white, purple],[red, blue, white, purple],[red, red, white, Empty]),
    ([blue, green, white, purple],[red, blue, white, yellow],[red, white, Empty, Empty]),
    ([blue, green, white, purple],[red, blue, purple, yellow],[white, white, Empty, Empty]),
    ([blue, green, white, purple],[red, green, white, purple],[red, red, red, Empty]),
    ([blue, green, white, purple],[red, green, white, yellow],[red, red, Empty, Empty]),
    ([blue, green, white, purple],[red, green, purple, yellow],[red, white, Empty, Empty]),
    ([blue, green, white, purple],[red, white, purple, yellow],[white, white, Empty, Empty]),
    ([blue, green, white, purple],[blue, green, white, purple],[red, red, red, red]),
    ([blue, green, white, purple],[blue, green, white, yellow],[red, red, red, Empty]),
    ([blue, green, white, purple],[blue, green, purple, yellow],[red, red, white, Empty]),
    ([blue, green, white, purple],[blue, white, purple, yellow],[red, white, white, Empty]),
    ([blue, green, white, purple],[green, white, purple, yellow],[white, white, white, Empty]),
    ([blue, green, white, yellow],[red, blue, green, white],[white, white, white, Empty]),
    ([blue, green, white, yellow],[red, blue, green, purple],[white, white, Empty, Empty]),
    ([blue, green, white, yellow],[red, blue, green, yellow],[red, white, white, Empty]),
    ([blue, green, white, yellow],[red, blue, white, purple],[red, white, Empty, Empty]),
    ([blue, green, white, yellow],[red, blue, white, yellow],[red, red, white, Empty]),
    ([blue, green, white, yellow],[red, blue, purple, yellow],[red, white, Empty, Empty]),
    ([blue, green, white, yellow],[red, green, white, purple],[red, red, Empty, Empty]),
    ([blue, green, white, yellow],[red, green, white, yellow],[red, red, red, Empty]),
    ([blue, green, white, yellow],[red, green, purple, yellow],[red, red, Empty, Empty]),
    ([blue, green, white, yellow],[red, white, purple, yellow],[red, white, Empty, Empty]),
    ([blue, green, white, yellow],[blue, green, white, purple],[red, red, red, Empty]),
    ([blue, green, white, yellow],[blue, green, white, yellow],[red, red, red, red]),
    ([blue, green, white, yellow],[blue, green, purple, yellow],[red, red, red, Empty]),
    ([blue, green, white, yellow],[blue, white, purple, yellow],[red, red, white, Empty]),
    ([blue, green, white, yellow],[green, white, purple, yellow],[red, white, white, Empty]),
    ([blue, green, purple, yellow],[red, blue, green, white],[white, white, Empty, Empty]),
    ([blue, green, purple, yellow],[red, blue, green, purple],[white, white, white, Empty]),
    ([blue, green, purple, yellow],[red, blue, green, yellow],[red, white, white, Empty]),
    ([blue, green, purple, yellow],[red, blue, white, purple],[white, white, Empty, Empty]),
    ([blue, green, purple, yellow],[red, blue, white, yellow],[red, white, Empty, Empty]),
    ([blue, green, purple, yellow],[red, blue, purple, yellow],[red, red, white, Empty]),
    ([blue, green, purple, yellow],[red, green, white, purple],[red, white, Empty, Empty]),
    ([blue, green, purple, yellow],[red, green, white, yellow],[red, red, Empty, Empty]),
    ([blue, green, purple, yellow],[red, green, purple, yellow],[red, red, red, Empty]),
    ([blue, green, purple, yellow],[red, white, purple, yellow],[red, red, Empty, Empty]),
    ([blue, green, purple, yellow],[blue, green, white, purple],[red, red, white, Empty]),
    ([blue, green, purple, yellow],[blue, green, white, yellow],[red, red, red, Empty]),
    ([blue, green, purple, yellow],[blue, green, purple, yellow],[red, red, red, red]),
    ([blue, green, purple, yellow],[blue, white, purple, yellow],[red, red, red, Empty]),
    ([blue, green, purple, yellow],[green, white, purple, yellow],[red, red, white, Empty]),
    ([blue, white, purple, yellow],[red, blue, green, white],[white, white, Empty, Empty]),
    ([blue, white, purple, yellow],[red, blue, green, purple],[white, white, Empty, Empty]),
    ([blue, white, purple, yellow],[red, blue, green, yellow],[red, white, Empty, Empty]),
    ([blue, white, purple, yellow],[red, blue, white, purple],[white, white, white, Empty]),
    ([blue, white, purple, yellow],[red, blue, white, yellow],[red, white, white, Empty]),
    ([blue, white, purple, yellow],[red, blue, purple, yellow],[red, red, white, Empty]),
    ([blue, white, purple, yellow],[red, green, white, purple],[white, white, Empty, Empty]),
    ([blue, white, purple, yellow],[red, green, white, yellow],[red, white, Empty, Empty]),
    ([blue, white, purple, yellow],[red, green, purple, yellow],[red, red, Empty, Empty]),
    ([blue, white, purple, yellow],[red, white, purple, yellow],[red, red, red, Empty]),
    ([blue, white, purple, yellow],[blue, green, white, purple],[red, white, white, Empty]),
    ([blue, white, purple, yellow],[blue, green, white, yellow],[red, red, white, Empty]),
    ([blue, white, purple, yellow],[blue, green, purple, yellow],[red, red, red, Empty]),
    ([blue, white, purple, yellow],[blue, white, purple, yellow],[red, red, red, red]),
    ([blue, white, purple, yellow],[green, white, purple, yellow],[red, red, red, Empty]),
    ([green, white, purple, yellow],[red, blue, green, white],[white, white, Empty, Empty]),
    ([green, white, purple, yellow],[red, blue, green, purple],[white, white, Empty, Empty]),
    ([green, white, purple, yellow],[red, blue, green, yellow],[red, white, Empty, Empty]),
    ([green, white, purple, yellow],[red, blue, white, purple],[white, white, Empty, Empty]),
    ([green, white, purple, yellow],[red, blue, white, yellow],[red, white, Empty, Empty]),
    ([green, white, purple, yellow],[red, blue, purple, yellow],[red, red, Empty, Empty]),
    ([green, white, purple, yellow],[red, green, white, purple],[white, white, white, Empty]),
    ([green, white, purple, yellow],[red, green, white, yellow],[red, white, white, Empty]),
    ([green, white, purple, yellow],[red, green, purple, yellow],[red, red, white, Empty]),
    ([green, white, purple, yellow],[red, white, purple, yellow],[red, red, red, Empty]),
    ([green, white, purple, yellow],[blue, green, white, purple],[white, white, white, Empty]),
    ([green, white, purple, yellow],[blue, green, white, yellow],[red, white, white, Empty]),
    ([green, white, purple, yellow],[blue, green, purple, yellow],[red, red, white, Empty]),
    ([green, white, purple, yellow],[blue, white, purple, yellow],[red, red, red, Empty]),
    ([green, white, purple, yellow],[green, white, purple, yellow],[red, red, red, red])
            ]

libTest :: IO ()
libTest = do 
    putStrLn ("Number of false cases: " ++ show (countFalseNumber (checkTestCase testCase)))