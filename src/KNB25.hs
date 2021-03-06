module KNB25(
  kNB25
)where
import Prav25
import System.Random
import Data.Array
import qualified Data.Set as S
                             
gArray :: Array Int String
gArray = listArray (1, 25) ["пистолет","динамит","ядерная бомба","молния",
                            "дьявол","дракон","инопланетянин","вода",
                            "кубок","воздух","луна","бумага","губка",
                            "волк","таракан","дерево","мужчина","женщина",
                            "обезьяна","змея","топор","ножницы","огонь",
                            "солнце","камень"]                             
                             
iAmWinner :: S.Set (Int,Int)
iAmWinner = S.fromList[(1,14),(1,15),(1,16),(1,17),(1,18),(1,19),
                       (1,20),(1,21),(1,22),(1,23),(1,24),(1,25),
                       (2,1),(2,15),(2,16),(2,17),(2,18),(2,19),
                       (2,20),(2,21),(2,22),(2,23),(2,24),(2,25),
                       (3,1),(3,2),(3,16),(3,17),(3,18),(3,19),
                       (3,20),(3,21),(3,22),(3,23),(3,24),(3,25),
                       (4,1),(4,2),(4,3),(4,17),(4,18),(4,19),
                       (4,20),(4,21),(4,22),(4,23),(4,24),(4,25),
                       (5,1),(5,2),(5,3),(5,4),(5,18),(5,19),
                       (5,20),(5,21),(5,22),(5,23),(5,24),(5,25),
                       (6,1),(6,2),(6,3),(6,4),(6,5),(6,19),
                       (6,20),(6,21),(6,22),(6,23),(6,24),(6,25),  
                       (7,1),(7,2),(7,3),(7,4),(7,5),(7,6),
                       (7,20),(7,21),(7,22),(7,23),(7,24),(7,25),
                       (8,1),(8,2),(8,3),(8,4),(8,5),(8,6),(8,7),
                       (8,21),(8,22),(8,23),(8,24),(8,25),
                       (9,1),(9,2),(9,3),(9,4),(9,5),(9,6),(9,7),
                       (9,8),(9,22),(9,23),(9,24),(9,25),  
                       (10,1),(10,2),(10,3),(10,4),(10,5),(10,6),
                       (10,7),(10,8),(10,9),(10,23),(10,24),(10,25),
                       (11,1),(11,2),(11,3),(11,4),(11,5),(11,6),
                       (11,7),(11,8),(11,9),(11,10),(11,24),(11,25),  
                       (12,1),(12,2),(12,3),(12,4),(12,5),(12,6),
                       (12,7),(12,8),(12,9),(12,10),(12,11),(12,25),
                       (13,1),(13,2),(13,3),(13,4),(13,5),(13,6),
                       (13,7),(13,8),(13,9),(13,10),(13,11),(13,12),
                       (14,2),(14,3),(14,4),(14,5),(14,6),(14,7),
                       (14,8),(14,9),(14,10),(14,11),(14,12),(14,13), 
                       (15,3),(15,4),(15,5),(15,6),(15,7),(15,8),
                       (15,9),(15,10),(15,11),(15,12),(15,13),(15,14),
                       (16,4),(16,5),(16,6),(16,7),(16,8),(16,9),
                       (16,10),(16,11),(16,12),(16,13),(16,14),(16,15),
                       (17,5),(17,6),(17,7),(17,8),(17,9),(17,10),
                       (17,11),(17,12),(17,13),(17,14),(17,15),(17,16),
                       (18,6),(18,7),(18,8),(18,9),(18,10),(18,11),
                       (18,12),(18,13),(18,14),(18,15),(18,16),(18,17),
                       (19,7),(19,8),(19,9),(19,10),(19,11),(19,12),
                       (19,13),(19,14),(19,15),(19,16),(19,17),(19,18),
                       (20,8),(20,9),(20,10),(20,11),(20,12),(20,13),
                       (20,14),(20,15),(20,16),(20,17),(20,18),(20,19), 
                       (21,9),(21,10),(21,11),(21,12),(21,13),(21,14),
                       (21,15),(21,16),(21,17),(21,18),(21,19),(21,20), 
                       (22,10),(22,11),(22,12),(22,13),(22,14),(22,15),
                       (22,16),(22,17),(22,18),(22,19),(22,20),(22,21), 
                       (23,11),(23,12),(23,13),(23,14),(23,15),(23,16),
                       (23,17),(23,18),(23,19),(23,20),(23,21),(23,22),
                       (24,12),(24,13),(24,14),(24,15),(24,16),(24,17),
                       (24,18),(24,19),(24,20),(24,21),(24,22),(24,23),
                       (25,13),(25,14),(25,15),(25,16),(25,17),(25,18),
                       (25,19),(25,20),(25,21),(25,22),(25,23),(25,24)]
                       
kNB25 :: Int -> Int -> IO ()
kNB25 a b = do
  gy <- prav25
  gi <- randomRIO (1::Int, 26::Int)
  if (gy>25 || gy<1) then do
     putStrLn $ concat ["Видимо,ты не хочешь играть,но,возможно,ты ",
                        "просто ошибся,поэтому спрошу.\n"           ]
     putStrLn "Играем ещё? \n\t1. да \n\t2. нет"
     otvet <- getLine
     let otv = read otvet :: Int
     if (otv==1) then do
       kNB25 a b
      else do 
        putStrLn "Вы закончили игру."
  else do
    if gi==26 then do 
      putStrLn "У меня фига! Хахаха! Я так пошутил!"
      kNB25 a b
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
          kNB25 a b  
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
            kNB25 (a+1) b  
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
            kNB25 a (b+1) 
          else do 
            putStrLn "Вы закончили игру."
