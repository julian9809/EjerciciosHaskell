division:: Int->Int->Int
division a 0 = 0
division 0 b = 0
division a b = if a < b then 0 else 1 + division (a-b) b
