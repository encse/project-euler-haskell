
powersWithLengthCheck x = takeWhile (lengthCheck x) [1..]

lengthCheck x n = (length . show $ x^n) == n

solve = sum $ map (length.powersWithLengthCheck) [1..9]