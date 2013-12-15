module Main where

import Parser
import EvalStack
import AST
import StdLib
import System.Environment (getArgs)
import Text.Trifecta (parseFromFile)
import qualified Data.Map as M
import Data.Maybe (fromJust)

lumpy :: String -> IO ()
lumpy arg = do
  r <- parseFromFile definitions arg
  case r of
    Nothing -> return ()
    Just rs -> putStrLn $ unlines $  toAssembly rs

toAssembly :: [Definition] -> [String]
toAssembly rs =  map ppOp $ compiler rs
 where compiler ls = [Goto mainLabel] ++ (compile 100 names $ map bindStdLib ls)
       names = M.fromList $ zip (map name rs) [1..]
       mainLabel = fromJust $ M.lookup "main" names 

main :: IO ()
main = mapM_ lumpy =<< getArgs

mapFst :: (a -> b) -> (a,c) -> (b,c)
mapFst f (a,b) = (f a,b)
