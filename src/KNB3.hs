 module KNB3(
   kNB3
)where
import Prav3
import System.Random
import Data.Array
import qualified Data.Set as S

gArray :: Array Int String
gArray = listArray (1, 3) ["камень", "ножницы", "бумага"]

iAmWinner :: S.Set (Int,Int)
iAmWinner = S.fromList[(1,2),(2,3),(3,1)]

kNB3 :: Int -> Int -> IO ()
kNB3 a b = do
  gy <- prav3
  gi <- randomRIO (1::Int, 4::Int)
  if (gy>3 || gy<1) then do
     putStrLn $ concat ["Видимо,ты не хочешь играть,но,возможно,ты ",
                        "просто ошибся,поэтому спрошу.\n"           ]
     putStrLn "Играем ещё? \n\t1. да \n\t2. нет"
     otvet <- getLine
     let otv = read otvet :: Int
     if (otv==1) then do
       kNB3 a b
      else do 
        putStrLn "Вы закончили игру."
  else do
    if gi==4 then do
      putStrLn "У меня фига! Хахаха! Я так пошутил!"
      kNB3 a b
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
        if (otv==1) then do
          kNB3 a b
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
          if (otv==1) then do
            kNB3 (a+1) b  
          else do 
            putStrLn "Вы закончили игру."
        else do 
          putStrLn "Ты выиграл!"
          putStrLn $ "Счёт прошедших игр: У меня " ++ show a ++ 
                     ", у тебя " ++ show (b+1) ++ ".\n"
          putStrLn "Играем ещё? \n\t1. да \n\t2. нет"
          otvet <- getLine
          let otv = read otvet :: Int
          if (otv==1) then do
            kNB3 a (b+1) 
          else do 
            putStrLn "Вы закончили игру."
        
