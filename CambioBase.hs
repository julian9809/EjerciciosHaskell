binario::Int->Int
binario 1 = 1
binario 0 = 0
binario a = binario(div a 2)*10+ (mod a 2) 

decimal::Int->Int 
decimal 0 = 0
decimal a = (mod a 10) + 2*decimal (div a 10)