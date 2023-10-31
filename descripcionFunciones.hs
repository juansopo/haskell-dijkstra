fromText :: String -> Bool -> Graph
-- Obtener un grafo ponderado a partir de una cadena de texto multilínea, donde cada línea especifica dos nodos y un peso.
-- Si los datos ya representan un grafo dirigido, simplemente pase los bordes, de lo contrario
-- agregue los bordes invertidos. Esto evita datos redundantes al trabajar con grafos no dirigidos.



fromList :: [((String, String), Float)] -> Graph
{- Crea una lista de nodos únicos en el grafo. es 
es una lista de tuplas que representan las aristas. 
fst . fst se utiliza para extraer el primer elemento de cada tupla 
(que contiene los dos nodos de la arista), y map (fst . fst) se aplica a cada tupla en es para obtener una lista 
de todos los nodos que aparecen en las aristas. 
La función nub se utiliza para eliminar duplicados y obtener una lista de nodos únicos.

"edgesFor es node" La función se utiliza para encontrar todas las aristas que están conectadas al nodo especificado.
connected es una lista de tuplas que contienen pares de nodos 
y pesos de aristas donde el primer nodo en cada par es igual al nodo especificado (node).
Luego, se utiliza map (\((_,n),wt) -> Edge n wt) connected para convertir estas tuplas en una lista de objetos Edge. 
Cada objeto Edge contiene el nodo al que está conectado la arista y el peso de la arista.

map (\n -> (n, edgesFor es n)) nodes: En esta parte, se aplica un mapeo a la lista de nodos únicos (nodes).
Para cada nodo en la lista, se crea una tupla que contiene el nombre del nodo 
y una lista de aristas que están conectadas a ese nodo. 
-}
fromList :: [((String, String), Float)] -> Graph
{-La función fromList toma una lista de pares ((String, String), Float) llamada es y crea un grafo a partir de ella. La función busca los nodos y las conexiones entre ellos en la lista es para construir un grafo.

La variable nodes se calcula utilizando nub . map (fst . fst) $ es. 
map (fst . fst) $ es toma la lista de pares es y extrae el primer elemento del primer par (String, String).
nub se aplica a esta lista para eliminar los elementos duplicados y conservar únicamente los nodos únicos.
La función edgesFor se define para obtener las aristas conectadas a un nodo dado en la lista es. 
Recibe dos argumentos: la lista es y un nodo (node), y realiza lo siguiente:

Filtra la lista es para encontrar todas las conexiones (pares ((n, _), _)) donde el primer elemento
del par coincide con el nodo node.
Mapea estos pares filtrados a objetos Edge, donde el segundo elemento del par se convierte en el peso de la arista.
La parte principal de la función fromList utiliza map para generar una lista de tuplas. Cada tupla contiene un nodo 
y la lista de aristas conectadas a ese nodo. El mapeo se realiza para cada nodo único encontrado en la lista nodes.
La estructura de cada tupla en la lista generada será (n, edgesFor es n).
 -}


edgesFor :: Graph -> Node -> [Edge]

{-La función filter se utiliza para encontrar todos los elementos en g que cumplan con la condición dada. 
En este caso, la condición es que el primer elemento de cada tupla (nd, _) en el grafo g sea igual al nodo n.
Después de aplicar filter, se utiliza head para obtener el primer elemento de la lista resultante. 
Esto selecciona la tupla que corresponde al nodo n en el grafo
Luego, se aplica snd a esta tupla seleccionada con head. La función snd se utiliza para obtener el segundo elemento de la tupla,
que es una lista de aristas conectadas al nodo n
-}

weightFor :: Node -> [Edge] -> Float
{-
La función filter se utiliza para encontrar todas las aristas en la lista que cumplan con la condición 
que el nodo n sea igual al nodo al que está conectada la arista (node e).
weight . head: Finalmente, se aplica la función weight al resultado obtenido con head. 
La función weight se utiliza para obtener el peso de la arista seleccionada.
IMPORTANTE weight y node lo puede utilizar ya que estan definidos en el preludio del tipo de dato Edge
weightFor queda a la espera que se le pase esa lista de aristas 
-}


dnodeForNode :: [Dnode] -> Node -> Dnode
{- filter (\(x, _) -> x == n) $ dnodes: Esta parte de la función utiliza la función filter
 para buscar dentro de la lista de nodos Dijkstra 
dnodes. 
La función filter toma una función como argumento que se utiliza para evaluar si un elemento de la lista cumple con  la condición  
que el primer elemento de cada tupla (x, _) 
en dnodes sea igual al nodo n que estamos buscando.
Se utiliza la función head para obtener ese nodo Dijkstra. 
Esto nos da el primer nodo Dijkstra en la lista que coincide con el nodo n
-}


dijkstra :: Graph -> Node -> [Dnode]
{-
Toma como entrada un grafo, representado como Graph, y un nodo de inicio, representado como Node,
y devuelve una lista de nodos Dijkstra ([Dnode])
que representan los nodos y sus distancias mínimas desde el nodo de inicio

se llama a la función initD con el grafo g y el nodo de inicio start para inicializar la lista de nodos Dijkstra. 
La función initD crea una lista de nodos Dijkstra con distancias iniciales a partir del nodo de inicio.

unchecked = map fst dnodes: Luego, se utiliza la función map para obtener una lista de nodos 
no verificados (sin revisar) a partir de la lista de nodos Dijkstra dnodes.
Esto se hace extrayendo el primer elemento (nodo) de cada nodo Dijkstra en la lista.

Finalmente, se llama a la función dijkstra' con el grafo g,
la lista de nodos Dijkstra dnodes y la lista de nodos no verificados unchecked

cuando se hace referencia a "nodos Dijkstra", se habla de los nodos del grafo que están siendo procesados 
por el algoritmo de Dijkstra para encontrar las distancias mínimas en un problema específico
-}

initD :: Graph -> Node -> [Dnode]
-- Dado un grafo y un nodo de inicio, construye una lista inicial de Dnodes

{-Itera a través de todos los nodos del grafo g utilizando la función map. 
Cada nodo del grafo se representa como una tupla (n, es) 
donde n es el nombre del nodo y es es la lista de aristas conectadas al nodo
Para cada nodo del grafo, se calcula la distancia mínima desde el nodo de inicio (start) hasta ese nodo.
Esto se hace en la función initDist. 
Si el nodo actual es igual al nodo de inicio, su distancia mínima se establece en 0 (porque es el nodo de inicio).
Si el nodo actual es directamente accesible desde el nodo de inicio (es decir, start está en la lista de nodos conectados connectedNodes es),
se establece la distancia mínima como el peso de la arista que conecta start con el nodo actual. 
Si el nodo actual no es directamente accesible desde el nodo de inicio, 
se establece la distancia mínima como un valor "infinito" representado como 1.0/0.0. 
Esto se hace para indicar que aún no se ha encontrado un camino hacia ese nodo.

Para cada nodo del grafo, se crea una tupla (n, ((initDist pr), start)), 
donde n es el nombre del nodo, (initDist pr) es la distancia mínima inicial calculada en el paso anterior y
start se utiliza para indicar que el nodo de inicio es el nodo previo en el camino más corto hacia este nodo 
en la etapa inicial.

La función map crea una lista de estas tuplas para todos los nodos del grafo,
lo que da como resultado la lista de nodos Dijkstra inicializada.
-}



dijkstra' :: Graph -> [Dnode] -> [Node] -> [Dnode]
{-
dijkstra' g dnodes [] = dnodes: La primera cláusula de la función establece el caso base de la recursión.
Si la lista de nodos no revisados unchecked está vacía, significa que se han revisado todos los nodos posibles, y en ese caso, 
la función simplemente devuelve la lista de nodos Dijkstra dnodes, que contendrá las distancias mínimas y caminos más cortos desde el nodo de inicio 
a todos los demás nodos del grafo.

dijkstra' g dnodes unchecked = ...: En la parte recursiva de la función, se realiza el siguiente proceso:

dunchecked = filter (\dn -> (fst dn) elem unchecked) dnodes: Se filtran los nodos Dijkstra en la lista 
dnodes para obtener solo aquellos que aún no han sido revisados, es decir, 
aquellos cuyos nombres (obtenidos con fst dn) están en la lista de nodos no revisados unchecked.


SE ORDENAN TODOS LOS NO CHEKEADOS

current = head . sortBy (\(_,(d1,_)) (_,(d2,_)) -> compare d1 d2) $ dunchecked: Se selecciona el nodo Dijkstra actual 
eligiendo el nodo con la distancia mínima en la lista de nodos Dijkstra no revisados. 
Para ello, se utiliza sortBy para ordenar la lista de dunchecked en función de la distancia mínima, 
y luego se toma el primer elemento con head. Este nodo se considera el nodo actual.

c = fst current: Se extrae el nombre del nodo actual.

unchecked' = delete c unchecked: Se elimina el nodo actual de la lista de nodos no revisados unchecked, ya que ahora se ha revisado.

edges = edgesFor g c: Se obtienen las aristas conectadas al nodo actual c.

cnodes = intersect (connectedNodes edges) unchecked': Se calcula la intersección entre los nodos conectados 
a través de las aristas del nodo actual y los nodos no revisados. 
Esto identifica los nodos conectados que aún no se han revisado.

dnodes' = map (\dn -> update dn current cnodes edges) dnodes: Se actualizan los nodos Dijkstra en la lista dnodes.
Para cada nodo Dijkstra en la lista, se utiliza la función update para verificar si se puede mejorar la distancia mínima
a través del nodo actual y, en caso afirmativo, se actualiza el nodo Dijkstra correspondiente.

Finalmente, la función dijkstra' se llama recursivamente con la nueva lista de nodos Dijkstra actualizada dnodes' y la nueva lista de nodos no revisados unchecked'.
Esto se repite hasta que no queden más nodos no revisados en unchecked, momento en el que se alcanza el caso base 
y se devuelve la lista de nodos Dijkstra final con las distancias mínimas 
y los caminos más cortos.-}




update :: Dnode -> Dnode -> [Node] -> [Edge] -> Dnode
-- Dado un Dnode para actualizar, el Dnode actual, los nodos conectados al actual
-- y las aristas del actual, devuelve un Dnode  actualizado.
{-
dn@(n, (nd, p)): El primer argumento dn es un nodo Dijkstra, que se descompone en tres partes:

n es el nombre del nodo.
(nd, p) es un par que contiene la distancia mínima (nd) y el nombre del nodo previo (p)
 en el camino más corto hacia este nodo.

(c, (cd, _)): El segundo argumento es otro nodo Dijkstra, 
que representa el nodo actual que se está considerando para actualizar el nodo dn.

c es el nombre del nodo actual.
(cd, _) es un par que contiene la distancia mínima (cd) desde el nodo de inicio hasta el nodo actual.
cnodes: Una lista de nodos conectados desde el nodo actual c.

edges: Una lista de aristas conectadas al nodo actual c.

La función realiza la siguiente lógica:

wt = weightFor n edges: Se obtiene el peso de la arista que conecta 
el nodo n al nodo actual c utilizando la función weightFor.
Esto se almacena en la variable wt.

Se verifica si el nodo n no está en la lista de nodos conectados
cnodes desde el nodo actual c.
Si es así, significa que no hay una conexión directa desde el nodo actual al nodo n, 
por lo que no se puede actualizar su distancia mínima. 
En este caso, la función devuelve el nodo Dijkstra original dn sin cambios.

Si el nodo n está en la lista de nodos conectados cnodes, 
se verifica si la suma de la distancia mínima actual desde el nodo de inicio hasta el nodo actual (cd) 
y el peso de la arista (wt) es menor que la distancia mínima anterior almacenada en el nodo dn (nd). 
Si esta suma es menor, significa que se ha encontrado un camino más corto hacia el nodo n a través del nodo actual c.
En este caso, la función devuelve un nuevo nodo Dijkstra que contiene la nueva distancia mínima y el nombre del nodo previo en el camino más corto.

Si ninguna de las condiciones anteriores se cumple,
la función devuelve el nodo Dijkstra original dn sin cambios.
-}





pathToNode :: [Dnode] -> Node -> [Node]
-- Dada una solución de Dijkstra y un nodo de destino, devuelve el camino hacia él.
{-
dn@(n, (d, p)) = dnodeForNode dnodes dest: Se utiliza la función dnodeForNode para buscar el nodo Dijkstra 
correspondiente al nodo de destino dest en la lista de nodos Dijkstra dnodes. 
Esto devuelve una tupla que contiene el nombre del nodo n, la distancia mínima d y el nombre del nodo previo p en el camino más corto.

A continuación, se verifica si el nombre del nodo n es igual al nombre del nodo previo p.
Si son iguales, significa que el nodo de destino dest es el mismo que el nodo de inicio, 
y ya se ha alcanzado el nodo de destino. En este caso, se devuelve una lista que contiene solo el nodo de destino dest.

Si el nombre del nodo n no es igual al nombre del nodo previo p, significa que todavía no se ha alcanzado el nodo de destino y se necesita un camino más largo. 
En este caso, se realiza una llamada recursiva a la función pathToNode
para encontrar el camino desde el nodo de inicio hasta el nodo previo p, y luego se agrega el nodo n al final de la lista. Esto se hace utilizando el operador ++ para concatenar la lista del camino desde el nodo de inicio hasta p con una lista que contiene solo el nodo n.

Este proceso se repite recursivamente hasta que se llega al nodo de destino, y finalmente se obtiene la lista de nodos que representan el camino más corto desde el nodo de inicio hasta el nodo de destino.
-}

pesoRutaAlNodo :: [Dnode] -> Node -> Float

{-
Da el peso del camino más corto hasta el nodo de destino
-}