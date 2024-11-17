-- PERF: first exercise, it was banal
-- Definire una funzione [ showHex :: Int -> String ] che, applicata a un numero intero,
-- ne restituisca la rappresentazione in base 16. Fare attenzione alla possibilità che
-- il numero sia negativo. Suggerimento: definire una funzione ausiliaria per
-- convertire un numero intero compreso tra 0 e 15 (estremi inclusi)
-- nel carattere corrispondente in base 16.

showHe :: Int -> String
showHe x
  | x < 16 = "0x" ++ aux x
  | otherwise = showHe (x `div` 16) ++ aux (x `mod` 16)
  where
    aux 0 = "0"
    aux 1 = "1"
    aux 2 = "2"
    aux 3 = "3"
    aux 4 = "4"
    aux 5 = "5"
    aux 6 = "6"
    aux 7 = "7"
    aux 8 = "8"
    aux 9 = "9"
    aux 10 = "A"
    aux 11 = "B"
    aux 12 = "C"
    aux 13 = "D"
    aux 14 = "E"
    aux 15 = "F"

-- PERF: second exercise, it was meh
-- enza fare uso di funzioni della libreria standard ad eccezione di ord e chr,
-- definire una funzione [ readHex :: String -> Int ] che, applicata a una stringa di cifre esadecimali,
-- restituisca il numero intero (non negativo) corrispondente.
-- Prestare attenzione alla possibilità che le cifre decimali siano minuscole o maiuscole

readHe :: String -> Int
readHe [] = 0
readHe (x : xs)
  | (x == '0') || (x == 'x') = readHe xs
  | otherwise = (aux x * (16 ^ length xs)) + readHe xs
  where
    aux '0' = 0
    aux '1' = 1
    aux '2' = 2
    aux '3' = 3
    aux '4' = 4
    aux '5' = 5
    aux '6' = 6
    aux '7' = 7
    aux '8' = 8
    aux '9' = 9
    aux 'A' = 10
    aux 'B' = 11
    aux 'C' = 12
    aux 'D' = 13
    aux 'E' = 14
    aux 'F' = 15
