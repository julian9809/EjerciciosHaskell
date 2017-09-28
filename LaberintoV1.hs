module Laberinto where 

matxy :: [[Char]]->(Int,Int)->Char
matxy a (fila,columna) =  (a!!fila)!!columna

mover::(Int,Int)->(Int,Int)
mover (fila,columna) = if (columna < 2) then (fila,columna+1) else if (columna == 2) then (fila+1,0) else (fila,columna)

encontrarInicio:: [[Char]]->(Int,Int)->(Int,Int)
encontrarInicio a (fila,columna)
    | matxy a (fila,columna) == 'x' = (fila,columna)
    | matxy a (fila,columna) /= 'x' = encontrarInicio a (mover(fila,columna))  







