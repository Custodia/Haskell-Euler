-- One liner bruteforce. 0.5 secs on my chromebook.
solution  = [a*b*(1000 - a - b) | a <- [1..333], b <- [(a+1)..(998-a-a)], a^2 + b^2 == (1000 - a - b)^2] !! 0