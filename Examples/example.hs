x :: Int
x=5

lucky :: (Integral a) => a -> String
lucky 7 = "El siete de la suerte :)"
lucky x = "Hoy no es tu dia de suerte :( "

factorial :: Integer -> Integer
factorial 0=1
factorial x=x * factorial (x-1)

sumatoria :: Integer -> Integer
sumatoria 0=1
sumatoria x= x + sumatoria (x-1)

sumEveryTwo :: [Integer] -> [Integer] 
sumEveryTwo [] = []
sumEveryTwo (x:[]) = [x]
sumEveryTwo (x:y:zs) = (x+y):sumEveryTwo zs

lengthList :: [Int] -> Int
lengthList[] = 0
lengthList(x:[]) = 1
lengthList(x:y:zs) = 2 + lengthList zs

peso :: Int -> [Char]
peso peso
	|peso <= 40 = "Estas muy flaco"
	|peso <= 60 = "Estas muy bien de peso"
	|peso <= 80 = "Estas como gordo"
	|otherwise = "Estas super gordo"

mayor :: Int -> Int -> Int
mayor b a
	|a > b = a
	|a < b = b
	|a == b = a

duplicrPares xs = [x*2 | x<-xs, (mod x 2) == 0 ]

