


fibonacci = [1,1,2] ++ fibi 1 2
	where		
		fibi x1 x2 = (x1 + x2) : (fibi x2 (x1 + x2))


solve = 1 + (length $ takeWhile (< 1000) $ map (length . show) fibonacci)
