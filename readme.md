Dijkstra's Algorithm (Version 2)
--------------------------------


Un gráfo ponderado se define aquí como una lista de pares, donde el primer elemento de cada par es un Nodo (solo un alias de tipo para cadena) y el segundo miembro es una lista de los Bordes incidentes en el Nodo. Dado que cada Edge en esta lista ya está asociado con el nodo, solo necesita registrar el nodo opuesto y el peso.

En esta versión, un grafo ponderado  no contiene ninguna información sobre el nodo inicial ni las distancias asociadas ni los predecesores de los nodos. Esa información se agrega a un nodo a través de la estructura 'Dnode' (un alias de tipo que asocia un par distancia/predecesor con un nodo).

Grafos dirigidos y no dirigidos

Se puede manejar grafos dirigidos y no dirigidos simplemente especificando un indicador "IsDirected" al leer un gráfico de un archivo. Si "IsDirected" es Verdadero, cada línea del archivo de entrada se asigna a un borde del gráfico. Si es falso, entonces para cada línea del archivo, el gráfico creado contendrá dos aristas, una en dirección hacia adelante y otra en dirección inversa. Esto ayuda a mantener los datos de entrada y el código SECO.

Sample Usage
------------

~~~
dow@dow-laptop ~/haskell/dijkstra $ ghci

Prelude> :l main.hs 
[1 of 2] Compiling Dijkstra         ( Dijkstra.hs, interpreted )
[2 of 2] Compiling Main             ( main.hs, interpreted )
Ok, modules loaded: Dijkstra, Main.

*Main> :browse Dijkstra 
data Edge = Edge {node :: Node, weight :: Float}
type Node = String
type Graph = [(Node, [Edge])]
type Dnode = (Node, (Float, Node))
fromText :: String -> Graph
edgesFor :: Graph -> Node -> [Edge]
dijkstra :: Graph -> Node -> [Dnode]
pathToNode :: [Dnode] -> Node -> [Node]
~~~


El segundo argumento de `fromText` especifica si el contenido del texto representa los bordes de un grafo **dirigido**.

~~~
*Main> txt <- readFile "graph2.txt"
*Main> let g = fromText txt False
~~~

Para ver los bordes incidentes en un nodo específico, use `edgesFor`

~~~
*Main> edgesFor g "c"
[Edge {node = "a", weight = 19.0},Edge {node = "d", weight = 30.0},Edge {node = "f", weight = 22.0},Edge {node = "h", weight = 15.0},Edge {node = "j", weight = 2.0},Edge {node = "m", weight = 7.0}]
~~~

Para obtener la solución, simplemente pase su gráfico y un nodo de inicio a `dijkstra`

~~~
*Main> let soln = dijkstra g "a"
*Main> soln
[("a",(0.0,"a")),("b",(12.0,"a")),("c",(13.0,"m")),("k",(3.0,"a")),("m",(6.0,"a")),("d",(7.0,"k")),("i",(13.0,"e")),("l",(19.0,"d")),("j",(15.0,"c")),("f",(9.0,"m")),("h",(28.0,"c")),("e",(10.0,"f")),("g",(11.0,"d"))]
~~~

Para rastrear una ruta hacia un nodo de destino específico, use `pathToNode`

~~~
*Main> pathToNode soln "h"
["a","m","c","h"]
~~~

Para rastrear el peso a una ruta hacia un nodo de destino específico, use `pesoRutaAlNodo`

~~~
*Main> pesoRutaAlNodo  soln "h"
47.0
~~~

