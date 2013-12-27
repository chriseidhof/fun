module Main where

import Parser
import Analyze
import EvalStack
import AST
import StdLib
import Ops
import System.Environment (getArgs)
import Text.Trifecta (parseFromFile)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import System.IO

lumpy :: String -> IO ()
lumpy arg = do
  r <- parseFromFile definitions arg
  case r of
    Nothing -> return ()
    Just rs -> do putStrLn $ unlines $  toAssembly rs
                  hPutStr stderr $ show $ analysis rs

toAssembly :: [Definition] -> [String]
toAssembly rs =  map show $ compiler rs
 where compiler ls = [Goto mainLabel] ++ (compile 100 names $ map bindStdLib ls)
       mainLabel = functionLabel $ fromJust $ M.lookup "main" names 
       names = analysis rs

analysis rs = M.fromList $ map analyze $ zip rs [1..]
 where analyze (Definition name e, x) = (name, AnalysisResult x (analyzeArity e))


main :: IO ()
main = mapM_ lumpy =<< getArgs

mapFst :: (a -> b) -> (a,c) -> (b,c)
mapFst f (a,b) = (f a,b)
