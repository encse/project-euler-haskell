change 0 _ = 1
change n [] = 0
change n (p:ps) =
	if p > n 
	then change n ps
	else (change n (ps)) + (change (n-p) (p:ps))


solve = change 200 [1,2,5,10,20,50,100,200]