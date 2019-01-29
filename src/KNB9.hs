module KNB9(
  kNB9
)where
import Prav9
import System.Random
import Data.Array
import qualified Data.Set as S

gArray :: Array Int String
gArray = listArray (1, 9) ["камень", "ножницы", "бумага", "губка", "огонь", "вода", 
                           "воздух", "пистолет", "человек"]

iAmWinner :: S.Set (Int,Int)
iAmWinner = S.fromList[(1,2),(1,4),(1,5),(1,9),(2,3),(2,4),(2,7),(2,9),
                       (3,1),(3,6),(3,7),(3,8),(4,3),(4,6),(4,7),(4,8),
                       (5,2),(5,3),(5,4),(5,9),(6,1),(6,2),(6,5),(6,8),
                       (7,1),(7,5),(7,6),(7,8),(8,1),(8,2),(8,5),(8,9),
                       (9,3),(9,4),(9,6),(9,7)]

kNB9 :: Int -> Int -> IO ()
kNB9 a b = do
  gy <- prav9
  gi <- randomRIO (1::Int, 10::Int)
  if (gy>9 || gy<1) then do
     putStrLn $ concat ["Видимо,ты не хочешь играть,но,возможно,ты",
                        "просто ошибся,поэтому спрошу.\n"          ]
     putStrLn "Играем ещё? \n\t1. да \n\t2. нет"
     otvet <- getLine
     let otv = read otvet :: Int
     if (otv==1) then do
       kNB9 a b
      else do 
        putStrLn "Вы закончили игру."
  else do
    if gi==10 then do
      putStrLn "У меня фига! Хахаха! Я так пошутил!"
      kNB9 a b
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
          kNB9 a b
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
            kNB9 (a+1) b  
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
            kNB9 a (b+1) 
          else do 
            putStrLn "Вы закончили игру."
