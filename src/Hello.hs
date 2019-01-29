module Hello(
  hello
)where

hello :: IO Int 
hello = do
  putStrLn $ concat ["Привет! Давай играть в камень-ножницы-бумага."        ,
                     "Скажи, в игру с каким количеством\n жестов ты хочешь ",
                     "поиграть из предложенных:3, 4, 5, 7, 9, 15, 25?"      ]
  cif <- getLine
  let number = read cif :: Int
  return (number)
