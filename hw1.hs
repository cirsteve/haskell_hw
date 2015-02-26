toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n < 1 = []
    | otherwise = mod n 10 : toDigitsRev(div n 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse(toDigitsRev n)

dblEveryOther :: [Integer] -> [Integer]
dblEveryOther [] = []
dblEveryOther (z:x:xs) = z : 2 * x : dblEveryOther(xs)

dblEveryOtherRL :: [Integer] -> [Integer]
dblEveryOtherRL [] = []
dblEveryOtherRL (x:[]) = [x]
dblEveryOtherRL (x:xs) = reverse(dblEveryOther(reverse(x)))
