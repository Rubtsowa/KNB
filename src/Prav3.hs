module Prav3(
  prav3
)where

prav3 :: IO Int 
prav3 = do
  putStrLn $ concat ["Немного правил:камень тупит ножницы, ножницы режут," ,
                     "бумагу,\n бумага заворачивает камень.\n"             ,
                     "Введи номер жеста,который ты показываешь"            ,
                     "(мой ответ от твоего "                               ,
                     "не зависит):\n\t1. камень\n\t2. ножницы\n\t3. бумага"]
  gesty <- getLine
  let gy = read gesty :: Int
  return (gy)
