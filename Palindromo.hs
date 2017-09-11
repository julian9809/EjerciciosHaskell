palindromo::Int->Int
palindromo 0 = 1 
palindromo a = if a<10 then 1 else if mod a 10 == primero a then palindromo (mod (a-ultimo a) 10) else 0

ultimo::Int->Int 
ultimo a = if a<10 then a else (ultimo (div a 10))*10

primero::Int->Int
primero a = if a<10 then a else (primero (div a 10))
