isPythagorean :: (Num a, Eq a) => (a,a,a) -> Bool
isPythagorean (a, b, c) = a * a + b * b == c * c

solve =
	let 
		triplets = [(a,b,c) | a<-[1..1000], b<-[a..1000], let c = 1000-a-b, c >= max a b]
		pythagorean = filter isPythagorean triplets
		(a,b,c) = head pythagorean 
	in 
		a * b * c