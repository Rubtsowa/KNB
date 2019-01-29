module KNB15(
  kNB15
)where
import Prav15
import System.Random
import Data.Array
import qualified Data.Set as S
                               
gArray :: Array Int String
gArray = listArray (1, 15) ["камень", "пистолет", "молния", "дьявол", "дракон", 
                            "вода", "воздух", "бумага", "губка", "волк", "дерево",
                            "человек", "змея", "ножницы", "огонь"]
                             
iAmWinner :: S.Set (Int,Int)
iAmWinner = S.fromList[(1,9),(1,10),(1,11),(1,12),(1,13),(1,14),(1,15),
                       (2,10),(2,11),(2,12),(2,13),(2,14),(2,15),(2,1), 
                       (3,11),(3,12),(3,13),(3,14),(3,15),(3,1),(3,2),
                       (4,12),(4,13),(4,14),(4,15),(4,1),(4,2),(4,3), 
                       (5,13),(5,14),(5,15),(5,1),(5,2),(5,3),(5,4),
                       (6,14),(6,15),(6,1),(6,2),(6,3),(6,4),(6,5),
                       (7,15),(7,1),(7,2),(7,3),(7,4),(7,5),(7,6),
                       (8,1),(8,2),(8,3),(8,4),(8,5),(8,6),(8,7), 
                       (9,2),(9,3),(9,4),(9,5),(9,6),(9,7),(9,8),
                       (10,3),(10,4),(10,5),(10,6),(10,7),(10,8),(10,9), 
                       (11,4),(11,5),(11,6),(11,7),(11,8),(11,9),(11,10),
                       (12,5),(12,6),(12,7),(12,8),(12,9),(12,10),(12,11), 
                       (13,6),(13,7),(13,8),(13,9),(13,10),(13,11),(13,12),
                       (14,7),(14,8),(14,9),(14,10),(14,11),(14,12),(14,13), 
                       (15,8),(15,9),(15,10),(15,11),(15,12),(15,13),(15,14)]

kNB15 :: Int -> Int -> IO ()
kNB15 a b = do
  gy <- prav15
  gi <- randomRIO (1::Int, 16::Int)
  if (gy>15 || gy<1) then do
     putStrLn $ concat ["Видимо,ты не хочешь играть,но,возможно,ты",
                        "просто ошибся,поэтому спрошу.\n"          ]
     putStrLn "Играем ещё? \n\t1. да \n\t2. нет"
     otvet <- getLine
     let otv = read otvet :: Int
     if (otv==1) then do
       kNB15 a b
      else do 
        putStrLn "Вы закончили игру."
  else do
    if gi==16 then do 
      putStrLn "У меня фига! Хахаха! Я так пошутил!"
      kNB15 a b
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
          kNB15 a b  
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
            kNB15 (a+1) b  
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
            kNB15 a (b+1) 
          else do 
             putStrLn "Вы закончили игру."
