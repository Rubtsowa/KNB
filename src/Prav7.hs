module Prav7(
  prav7
)where

prav7 :: IO Int 
prav7 = do
  putStrLn $ concat ["Немного правил:Камень тупит ножницы, камень давит губку,"  ,
                     "камень тушит огонь,\n ножницы режут бумагу, ножницы"       ,
                     " режут губку,ножницы рассекают воздух,\n бумага"           ,
                     " заворачивает камень,бумага рассекает воздух, бумага"      ,
                     " перекрывает воду,\n огонь сжигает бумагу, огонь "         ,
                     "сжигает губку, огонь опаляет ножницы,\nгубка "             ,
                     "впитывает воду, губка пропускает воздух, губка истирает "  ,
                     "бумагу,\n воздух волнует воду, воздух тушит огонь, воздуx ",
                     "истирает камень,\n вода топит камень, вода тушит огонь, "  ,
                     "от воды ножницы ржавеют.\n"                                ,
                     "Введи номер жеста,который ты показываешь"                  ,
                     "(мой ответ от твоего не зависит):"                         ,
                     "\n\t1. камень\n\t2. ножницы\n\t3. бумага\n"                ,
                     "\t4. губка\n\t5. огонь\n\t6. вода\n\t7. воздух"            ]
  gesty <- getLine
  let gy = read gesty :: Int
  return (gy)
