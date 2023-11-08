module Test
  ( test
  , describe
  ) where

green = "\ESC[32m"

red = "\ESC[31m"

reset = "\ESC[0m"

color code s = code ++ s ++ reset

test :: (Eq b, Show b) => (a -> b) -> String -> a -> b -> IO ()
test runner description input expectedResult =
  let result = runner input
   in if result == expectedResult
        then putStrLn $ color green $ " ✓ " ++ description
        else do
          putStrLn $ color red $ " 𐄂 " ++ description
          putStrLn $ color green "   Expected:"
          putStrLn $ "    " ++ show expectedResult
          putStrLn $ color red "   Received:"
          putStrLn $ "    " ++ show result
          putStrLn ""

describe :: String -> [IO ()] -> IO ()
describe thing tests = do
  putStrLn thing
  sequence_ tests
