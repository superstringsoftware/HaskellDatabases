module Main where

import Muon.Data.Column

main :: IO ()
main = putStrLn "This is the db" >> prettyPrintCT ctable
