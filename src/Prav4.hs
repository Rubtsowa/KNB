module Prav4(
  prav4
)where

prav4 :: IO Int 
prav4 = do
  putStrLn $ concat ["Немного правил:камень тупит ножницы, ножницы режут "   ,
                     "бумагу,\nбумага заворачивает камень, бумага"           ,
                     " покрывает колодец, камень тонет в колодце,\n"         ,
                     " ножницы разбиваются об колодец.\n"                    ,
                     "Введи номер жеста,который ты показываешь"              ,
                     " (мой ответ от твоего не зависит):"                    , 
                     "\n\t1. камень\n\t2. ножницы\n\t3. бумага\n\t4. колодец"]
  gesty <- getLine
  let gy = read gesty :: Int
  return (gy)
