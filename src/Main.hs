module Main where
import Hello
import KNB3
import KNB4
import KNB5
import KNB7
import KNB9
import KNB15
import KNB25

main :: IO ()
main = do
  number <- hello
   
  case number of
    3  -> g3
    4  -> g4
    5  -> g5
    7  -> g7
    9  -> g9
    15 -> g15
    25 -> g25
    _  -> g3

g3 :: IO ()
g3 = do
  putStrLn "\"Играем 3 жестами\""
  kNB3 0 0
    
g4 :: IO ()
g4 = do
  putStrLn "\"Играем 4 жестами\""
  kNB4 0 0

g5 :: IO ()
g5 = do
  putStrLn "\"Играем 5 жестами\""
  kNB5 0 0
    
g7 :: IO ()
g7 = do
  putStrLn "\"Играем 7 жестами\""
  kNB7 0 0 
    
g9 :: IO ()
g9 = do
  putStrLn "\"Играем 9 жестами\""
  kNB9 0 0 
  
g15 :: IO ()
g15 = do
  putStrLn "\"Играем 15 жестами\""
  kNB15 0 0
  
g25 :: IO ()
g25 = do
  putStrLn "\"Играем 25 жестами\""
  kNB25 0 0
    
