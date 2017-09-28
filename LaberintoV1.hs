module Laberinto where 

matxy :: [[Char]]->(Int,Int)->Char
matxy a (fila,columna) =  (a!!fila)!!columna

mover:: [[Char]]->(Int,Int)->(Int,Int)
mover (a:as) (fila,columna) = if (columna < ((length a)-1)) then (fila,columna+1) else if (columna == ((length a)-1)) then (fila+1,0) else (fila,columna)

encontrarInicio:: [[Char]]->(Int,Int)->(Int,Int)
encontrarInicio a (fila,columna)
    | matxy a (fila,columna) == 'x' = (fila,columna)
    | matxy a (fila,columna) /= 'x' = encontrarInicio a (mover a (fila,columna))  







