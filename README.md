# KNB
игра "камень-ножницы-бумага"
Текстовая игра «камень-ножницы-бумага» на разное количество жестов.
Игра написана на функциональном языке программирования Haskell.

Чтобы запустить её и посмотреть, как она работает, нужно:
1)	Установить на компьютер HaskellPlatform
2)	Скачать файлы
3)	В FarManager открыть папку со скачанными файлами
4)	Ввести «cabal build»
Таким образом, в папке помимо папки src появится папка dist.
Далее, чтобы запустить игру в FarManager нужно открыть: 
1)	dist
2)	build
3)	KNB
4)	KNB.exe

Когда будет запущен файл KNB.exe высветится приглашение к игре, а также возможные варианты количества жестов. После выбора варианта игры, появится сообщение с правилами игры (кто кого выигрывает) и запрос на жест, который вы показываете. Компьютер, выбором случайного числа, тоже выбирает какой-то жест. У компьютера есть возможность шутить, чего нет у игрока. Когда Вы и компьютер выберете жесты, высветится сообщение кто чего показал и какой результат игры, также, последует вопрос «хотите Вы играть дальше или нет». Если Вы продолжаете играть, то очки за игры будут суммироваться и выводиться на экран после каждого прохождения.   


