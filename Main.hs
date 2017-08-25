module Main where
import System.Environment
import Parse

main :: IO ()
main = getArgs >>= \args -> putStrLn (args !! 0) >> putStrLn (Parse.readExpr (args !! 0) )
