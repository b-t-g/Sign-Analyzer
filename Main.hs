module Main where
import System.Environment
import Parse

main :: IO ()
main = getArgs >>= \args -> if length args > 1 && head args == "-f" then
                              readFile(args !! 1) >>= \file -> putStrLn(Parse.readExpr file)
                            else
                              if length args == 1 then
                                putStrLn (Parse.readExpr (head args))
                              else
                                mapM_ putStr ["Input does not match expected format.\n",
                                              "Expecting either -f followed by a file ",
                                              "or a single argument consisting of a program.\n"]
