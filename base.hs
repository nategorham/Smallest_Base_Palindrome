convertBase :: Integer -> Integer -> [Integer]
convertBase 0 _ = []
convertBase 1 _ = []
convertBase base 0 = []
convertBase base num = convertBase base (num `div` base) ++ [num `mod` base]

isPalindrome :: [Integer] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) = (x == last xs) && isPalindrome (init xs)

sbp :: Integer -> Integer ->(Integer, [Integer])
sbp 0 _ = error "Invalid input"
sbp 1 _ = error "Invalid input"
sbp base num | num > base = if isPalindrome cov then (base, cov) else sbp (base + 1) num
    where cov = convertBase base num
sbp b n = error (show n ++ " does not have a sbp")

--sbp' :: Integer -> Integer ->(Integer, [Integer])
--sbp' b n = 

threeToN :: Integer -> [(Integer, Integer)]
threeToN n | n >= 3 = threeToN (n - 1) ++ [(n, fst (sbp 2 n))]
threeToN _ = []