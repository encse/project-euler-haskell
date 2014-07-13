isPalindrome :: Int -> Bool

isPalindrome n =
	stPalindrome (show n)
	
	where 
		stPalindrome "" = True
 		stPalindrome st =
			let 
				chFirst = head st
				chLast = last st
				stMid = take ((length st) - 2) (drop 1 st) 
			in
				(length st) == 1 || chFirst == chLast && stPalindrome stMid
 			
solve =
	let 
		muls = [x*y | x <- [100..999], y <- [100..999]]
		palindromes = filter isPalindrome muls
	in
		maximum palindromes
		
