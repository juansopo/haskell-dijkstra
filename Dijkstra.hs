module Dijkstra 
(
  fromText,
  dijkstra,
  pathToNode,
  edgesFor,
  pesoRutaAlNodo, 
  Edge(..),
  Node,
  Graph,
  Dnode
) where

import Data.List

data Edge = Edge { node::Node, weight::Float } deriving (Show)
type Node = String
type Graph = [(Node, [Edge])]
type Dnode = (Node, (Float, Node))


fromText :: String -> Bool -> Graph
fromText strLines isDigraph = 
  let readData [n1, n2, w] = ((n1, n2), read w :: Float)
      es = map (readData . words) $ lines strLines
      allEs = if isDigraph then es 
              else appendReversed es
  in fromList allEs

appendReversed :: [((String, String), Float)] -> [((String, String), Float)]
appendReversed es = es ++ map (\((n1,n2),w) -> ((n2,n1),w)) es

-- Toma una lista de pares donde el primer elemento es una lista de dos nodos en cualquier orden 
--y el segundo elemento es el peso para la arista que los conecta.
fromList :: [((String, String), Float)] -> Graph
fromList es =
  let nodes = nub . map (fst . fst) $ es
      edgesFor es node = 
        let connected = filter (\((n,_),_) -> node == n) $ es
        in map (\((_,n),wt) -> Edge n wt) connected 
  in map (\n -> (n, edgesFor es n)) nodes


-- Dado un grafo ponderado y un nodo, devuelve las aristas incidentes al nodo.
edgesFor :: Graph -> Node -> [Edge]

edgesFor g n = snd . head . filter (\(nd, _) -> nd == n) $ g

-- Dado un nodo y una lista de aristas, una de las cuales es incidente en el nodo, devuelve el peso.
weightFor :: Node -> [Edge] -> Float
weightFor n = weight . head . filter (\e -> n == node e)

-- Dado una lista de aristas, devuelve sus nodos.
connectedNodes :: [Edge] -> [Node]
connectedNodes = map node


dnodeForNode :: [Dnode] -> Node -> Dnode
dnodeForNode dnodes n = head . filter (\(x, _) -> x == n) $ dnodes


dijkstra :: Graph -> Node -> [Dnode]
dijkstra g start = 
  let dnodes = initD g start
      unchecked = map fst dnodes
  in  dijkstra' g dnodes unchecked

-- Dado un grafo y un nodo de inicio, construye una lista inicial de Dnodes
initD :: Graph -> Node -> [Dnode]
initD g start =
  let initDist (n, es) = 
        if n == start 
        then 0 
        else if start `elem` connectedNodes es
             then weightFor start es
             else 1.0/0.0
  in map (\pr@(n, _) -> (n, ((initDist pr), start))) g


-- Algoritmo de Dijkstra .

dijkstra' :: Graph -> [Dnode] -> [Node] -> [Dnode]
dijkstra' g dnodes [] = dnodes
dijkstra' g dnodes unchecked = 
  let dunchecked = filter (\dn -> (fst dn) `elem` unchecked) dnodes
      current = head . sortBy (\(_,(d1,_)) (_,(d2,_)) -> compare d1 d2) $ dunchecked
      c = fst current
      unchecked' = delete c unchecked
      edges = edgesFor g c
      cnodes = intersect (connectedNodes edges) unchecked'
      dnodes' = map (\dn -> update dn current cnodes edges) dnodes
  in dijkstra' g dnodes' unchecked' 


update :: Dnode -> Dnode -> [Node] -> [Edge] -> Dnode
update dn@(n, (nd, p)) (c, (cd, _)) cnodes edges =
  let wt = weightFor n edges
  in  if n `notElem` cnodes then dn
      else if cd+wt < nd then (n, (cd+wt, c)) else dn

-- Dada una solución de Dijkstra y un nodo de destino, devuelve el camino hacia él.
pathToNode :: [Dnode] -> Node -> [Node]
pathToNode dnodes dest = 
  let dn@(n, (d, p)) = dnodeForNode dnodes dest
  in if n == p then [n] else pathToNode dnodes p ++ [n]

 
pesoRutaAlNodo :: [Dnode] -> Node -> Float
pesoRutaAlNodo  dnodes dest = 
  let dn@(n, (d, p)) = dnodeForNode dnodes dest
  in if n == p then 0.0 else  d + pesoRutaAlNodo dnodes p 
