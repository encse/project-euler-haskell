change 0 _ = 1
change n [] = 0
change n (p:ps) =
	if p > n 
	then change n ps
	else (change n (ps)) + (change (n-p) (p:ps))
