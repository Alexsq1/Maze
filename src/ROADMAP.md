          
{-
Programar:
Búsqueda en anchura         Hecho
Búsqueda en profundidad     Hecho
IDDFS                       Hecho
A*                          Hecho
IDA*

-}

{-
FALTARÍA

A*
Contar heurísticas. Iterar sobre [ [(Square, Integer)] ] (path y distancia desde origen)
Al generar (expand, expandValid etc), calcular dist acumulada + heurística, y ordenar la lista según eso

IDA*
Va a haber que modificar el dfs, pero búsqueda es igual
Si dist acumulada + heur > dMax, podar (guardar el mínimo de los que se podan)
dMax inicial: heurística de inicio
Siguiente iteración: valor mínimo de los que se podaron (guardarlos)
Podría estar chulo explorar las ramas del dfs según heurística

-}
