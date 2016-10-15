--- Punto 1

--- Paso 1 
--- Retornar un entero como una lista invertida ejemplo 123 = [3,2,1]
toDigitRev :: Int => [Int]
toDigitRev 0 = []
toDigitRev x = [mod x 10] ++ toDigitRev(div x 10)

----------------------------------------------------------------------------------------------------------------------------------------------

--- Paso 2 
--- Retornar un entero como una lista ejemplo 123 = [1,2,3]
toDigit :: Int -> [Int]
toDigit = map (read . (:[])) . show

-----------------------------------------------------------------------------------------------------------------------------------------------

--- Paso 3
--- Retornar una lista donde los indices pares contados de atras hacia adelante se multipleque por 2 ejemplo [1,2,3,4] => [2,2,3,8]
doubleEveryOther :: [Int] -> [Int]
doubleEveryOther xs
	|length xs == 0 = []
	|length xs `mod` 2 == 0 = pair xs
	|otherwise = nonpair xs

pair :: [Int]->[Int]
pair []         = []
pair (x:xs:r)   = x * 2 : xs : doubleEveryOther r

nonpair :: [Int]->[Int]
nonpair (x:[])      = x:[]
nonpair (x:xs:r)    = x: xs * 2 : doubleEveryOther r

--------------------------------------------------------------------------------------------------------------------------------------------------

-- Paso 4
--- Retornar un numero que sera la suma de los items de una lista que entra como parametro, si los numero son compuestos, debera descoponerlo y sumar sus digitos
--- ejemplo [123, 12. 1] = 1+2+3+1+2+1 = 7
sumDigits :: [Int] -> Int
sumDigits [] = 0
sumDigits (x:[]) = sum (toDigitRev x) 
sumDigits (x:xs) = sum (toDigitRev x) + sumDigits xs


validate :: Int -> Bool
validate n
    |(sumDigits(doubleEveryOther(toDigit n))) `mod` 10 == 0    = True
    |otherwise  = False