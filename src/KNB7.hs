module KNB7(
  kNB7
)where
import Prav7 
import System.Random
import Data.Array
import qualified Data.Set as S
                              
gArray :: Array Int String
gArray = listArray (1, 7) ["камень", "ножницы", "бумага", "губка", "огонь", "вода", 
                           "воздух"]

iAmWinner :: S.Set (Int,Int)
iAmWinner = S.fromList[(1,2),(1,4),(1,5),(2,3),(2,4),(2,7),
                       (3,1),(3,6),(3,7),(4,3),(4,6),(4,7),
                       (5,2),(5,3),(5,4),(6,1),(6,2),(6,5),
                       (7,1),(7,5),(7,6)]

kNB7 :: Int -> Int -> IO ()
kNB7 a b = do
  gy <- prav7
  gi <- randomRIO (1::Int, 8::Int)
  if (gy>7 || gy<1) then do
     putStrLn $ concat ["Видимо,ты не хочешь играть,но,возможно,ты",
                        "просто ошибся,поэтому спрошу.\n"          ]
     putStrLn "Играем ещё? \n\t1. да \n\t2. нет"
     otvet <- getLine
     let otv = read otvet :: Int
     if (otv==1) then do
       kNB7 a b
      else do 
        putStrLn "Вы закончили игру."
  else do
    if gi==8 then do
      putStrLn "У меня фига! Хахаха! Я так пошутил!"
      kNB7 a b
    else do
      putStrLn $ concat ["У меня "   ,
                         gArray ! gi ,
                         ", у тебя " ,
                         gArray ! gy ,
                         "."         ]
      if gi==gy then do
        putStrLn "Ничья."
        putStrLn $ "Счёт прошедших игр: У меня " ++ show a ++ 
                   ", у тебя " ++ show b ++ ".\n"
        putStrLn "Играем ещё? \n\t1. да \n\t2. нет"
        otvet <- getLine
        let otv = read otvet :: Int
        if (otv==1) then
          kNB7 a b  
        else do 
          putStrLn "Вы закончили игру."
      else do
        if (gi,gy) `S.member` iAmWinner then do
          putStrLn "Я выиграл!"
          putStrLn $ "Счёт прошедших игр: У меня " ++ show (a+1) ++  
                     ", у тебя " ++ show b ++ ".\n"
          putStrLn "Играем ещё? \n\t1. да \n\t2. нет"
          otvet <- getLine
          let otv = read otvet :: Int
          if (otv==1) then
            kNB7 (a+1) b  
          else do 
            putStrLn "Вы закончили игру."
        else do
          putStrLn "Ты выиграл!"
          putStrLn $ "Счёт прошедших игр: У меня " ++ show a ++ 
                     ", у тебя " ++ show (b+1) ++ ".\n"
          putStrLn "Играем ещё? \n\t1. да \n\t2. нет"
          otvet <- getLine
          let otv = read otvet :: Int
          if (otv==1) then
            kNB7 a (b+1) 
          else do 
            putStrLn "Вы закончили игру."
