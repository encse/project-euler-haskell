
isLeapYear y 
	| y `div` 100 == 0 	= y `div` 400 == 0
	| otherwise			= y `div` 4 == 0

daysInYear y
	| isLeapYear y 	= 366
	| otherwise		= 365

daysInMonth y d 
	| elem d [1, 3, 5, 6, 8, 10, 12] = 31
	| d == 2 && isLeapYear y 		 = 29
	| d == 2 						 = 28
	| otherwise						 = 30

daysFromNewYear y 1 d = d - 1
daysFromNewYear y m d = (daysFromNewYear y (m-1) 1) + (daysInMonth y (m-1)) + d - 1

daysFromEpoch 1900 m d = daysFromNewYear 1900 m d
daysFromEpoch y m d = (daysFromNewYear y m d) + (daysInYear (y -1)) + (daysFromEpoch (y - 1) 1 1)

dow y m d = (daysFromEpoch y m d) `mod` 7

isSunday y m d =  dow y m d == 6

solve = length [1 | y <- [1901 .. 2000], m <- [1..12], isSunday y m 1] 


