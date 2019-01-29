module KNB5(
  kNB5
)where
import Prav5 
import System.Random
import Data.Array
import qualified Data.Set as S

gArray :: Array Int String
gArray = listArray (1, 5) ["камень", "ножницы", "бумага", "ящерица", "Спок"]
                         
iAmWinner :: S.Set (Int,Int)
iAmWinner = S.fromList[(1,2),(1,4),(2,3),(2,4),(3,1),(3,5),(4,3),(4,5),(5,1),(5,2)]

kNB5 :: Int -> Int -> IO ()
kNB5 a b = do
  gy <- prav5
  gi <- randomRIO (1::Int, 6::Int)
  if (gy>5 || gy<1) then do
     putStrLn $ concat ["Видимо,ты не хочешь играть,но,возможно,ты",
                        "просто ошибся,поэтому спрошу.\n"          ]
     putStrLn "Играем ещё? \n\t1. да \n\t2. нет"
     otvet <- getLine
     let otv = read otvet :: Int
     if (otv==1) then do
       kNB5 a b
      else do 
        putStrLn "Вы закончили игру."
  else do
    if gi==6 then do
      putStrLn "У меня фига! Хахаха! Я так пошутил!"
      kNB5 a b
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
          kNB5 a b  
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
            kNB5 (a+1) b  
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
            kNB5 a (b+1) 
          else do 
            putStrLn "Вы закончили игру."
