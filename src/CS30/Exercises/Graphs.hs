{-# LANGUAGE TemplateHaskell #-}
-- | This module doesn't really have an exercise, it is just there to show how to draw graphs.
--   The exported exercise 'stub' does only that: draw a graph with three vertices and two edges.
module CS30.Exercises.Graphs (graphStub, giveSet) where
import           CS30.Data
import           CS30.Exercises.Data
import           CS30.Exercises.SetBasics.SolutionChecker
import           Data.Aeson.TH
import           Data.List
import qualified Data.Map as Map
import           Debug.Trace
import           GHC.Stack

type Graph = ([Vertex],[Edge])
type Vertex = Int
type Edge = (Int,Int) -- (x,y) with x<y 
randoTree :: Int -> ChoiceTree Graph
randoTree n | n <= 1 = return ([0], [])
 | otherwise = do (_vs,es) <- randoTree n1
                  connection <- nodes [0..n1-1]
                  return ([0..n1],(connection,n1):es)
      where n1 = n - 1

getComponents :: Int -> Int -> ChoiceTree [Graph]
getComponents num_components vertices
 | num_components <= 1 = (:[]) <$> randoTree vertices
 | otherwise = do v <- pickNE [3*nc1 .. vertices - (nc2 * 3)]
                              [(nc1 * vertices) `div` num_components]
                  r1 <- getComponents nc1 v
                  r2 <- getComponents nc2 (vertices - v)
                  return $ r1 ++ r2
 where pickNE [] xs = nodes xs
       pickNE xs ys = Branch [nodes xs, nodes ys]
       nc1 = num_components `div` 2
       nc2 = num_components - nc1

-- | Generate a random graph without isolated vertices (if possible)
-- The degree of vertex 0 is generally expected to be somewhat higher than that of vertex 1,
-- so if you care about uniformity you should take a permutation of the graph first.
randomGraph :: Int -- ^ Total number of vertices in the graph. Is assumed to be at least num_components (larger graphs are returned if not).
            -> Int -- ^ Total number of components in the graph. This part is guaranteed by the algorithm. Use 1 to get a connected graph. (Typically 1 or 2)
            -> Int -- ^ Number of additional edges in the graph. Use 0 to get a cycle-free graph (a forest).
                   -- ^ Use a number similar to the number of components to get a highly sparse graph, and a number similar to the number of vertices minus the number of components to get a somewhat normal looking graph
                   -- ^ If this number is high, you will just end up with a disjoint union of clique graphs
            -> ChoiceTree Graph
randomGraph vertices num_components extra_edges
 = getComponents num_components vertices >>=
   (distribute extra_edges addEdge . map (addInfo edgeSpace)) >>=
   (return . joinComponents)

-- | As with randomGraph, but adds labels to all graph vertices
fullyLabeled :: Int -> Int -> Int -> ChoiceTree ([Vertex], [Char], [Edge])
fullyLabeled v n e
  = do (vs,es) <- randomGraph v n e
       p <- permuteCC (take v ['a'..])
       return (vs,p,es)

permuteCC :: [a] -> ChoiceTree [a]
permuteCC (x:xs)
 = do pm <- permuteCC xs
      i <- nodes [0..length pm]
      return (take i pm ++ x:drop i pm)
permuteCC [] = return []

edgeSpace :: Graph -> Int
edgeSpace (vs,es) = v * (v-1) `div` 2 - length es
  where v = length vs

addInfo :: (a -> b) -> a -> (a,b)
addInfo f a = (a,f a)

joinComponents :: [Graph] -> Graph
joinComponents [] = ([],[])
joinComponents [x] = x
joinComponents ((v,e):gs)
  = let (vs,es) = joinComponents gs
    in (v ++ map addMax vs,e ++ map addMax2 es)
  where maxV = 1 + maximum v
        addMax = (+ maxV)
        addMax2 (x,y) = (addMax x,addMax y)

distribute :: Int -> (a -> [Int] -> b) -> [(a,Int)] -> ChoiceTree [b]
distribute nr f lst' = go [] nr totsiz lst'
 where totsiz = sum . map snd $ lst'
       go _ _ _ [] = return []
       go acc select remaining ((v,lim):lst)
        | lim <= 0  = (:) (f v acc) <$> go [] select remaining lst
        | otherwise = lim1 `seq` remaining `seq` select `seq` id $
                      do draw <- (nodes [1..remaining])
                         (if draw <= select
                           then go (lim1:acc) (select - 1)
                           else go acc select
                           ) (remaining - 1) ((v,lim1):lst)
            where lim1 = lim - 1

-- | add the nth non-edges to the graph
addEdge :: HasCallStack => Graph -> [Int] -> Graph
addEdge (v,e) [] = (v,e)
addEdge (v,e) lst
  = (v, e++massNth lst nonEdges)
 where 
   nonEdges = [(v1,v2) | (v1:vs) <- tails v, v2<-vs, (v1,v2) `notElem` e]

massNth :: HasCallStack => [Int] -> [a] -> [a]
massNth = go 0
  where go i (x:xs) (a:as) | x <= i = a:go i xs (a:as)
          | otherwise = go (i+1) (x:xs) as
        go _ [] _ = []
        go _ _ _ = error "massNth: out of elements to pick from (index error)"

data GraphAnswerType
  = GAEdges [Field] [[String]] -- set of edges.
  | GAVertices [Field] [String] -- set of vertices.
$(deriveJSON defaultOptions ''GraphAnswerType)

-- | Graph basics questions:
-- a. Give the set of incident edges of the vertex a.
-- b. Give the set of neighbors of the vertex a.
-- c. Give the set of isolated vertices in graph G.
-- d. Give the set of all endpoints of the edge (u,v).
-- (Use roster notation for sets of edges or vertices, use pair notation for edges)
giveSet :: ExerciseType
giveSet = exerciseType "GraphsGiveSet" "(no lecture)" "Graph basics 1"
              [ do (v,l,e) <- fullyLabeled 10 1 7
                   let vl = zip v l
                   rndm <- nodes vl
                   -- trace (show vl) (return ())
                   let neighbors = sort ([v1 | (v1,v2) <- e, v2 == fst rndm]
                                       ++[v2 | (v1,v2) <- e, v1 == fst rndm])
                   let nbNames = massNth neighbors vl
                   -- trace ("Taking: "++show neighbors ++ "\n"++show nbNames) (return ())
                   return ( [ FText "Give the set of incident edges of the vertex "
                            , FMath [snd rndm],FText "."
                            , FGraph (JSGraph (map toFGNode vl) (map toFGEdge e)) mempty
                            , FNote "You can zoom and scroll in the graph."
                            ]
                          , GAEdges [FText "the set of incident edges"]
                                    [[[nm],[snd rndm]] | (_,nm) <- nbNames]
                          )
              , do (v,l,e) <- fullyLabeled 10 3 7
                   let vl = zip v l
                   rndm <- nodes vl
                   let neighbors = sort ([v1 | (v1,v2) <- e, v2 == fst rndm]
                                       ++[v2 | (v1,v2) <- e, v1 == fst rndm])
                   let nbNames = map (:[]) $ massNth neighbors l
                   return ( [ FText "Give the set of neighbors of the vertex "
                            , FMath [snd rndm],FText "."
                            , FGraph (JSGraph (map toFGNode vl) (map toFGEdge e))
                                     mempty
                            ]
                          , GAVertices [FText "the set of neighbors"] nbNames
                          )
              , do g <- randomGraph 7 2 3
                   extras <- nodes [2..6]
                   let (v,e) = joinComponents [([0..extras-1],[]), g]
                   p <- permuteCC (take (length v) ['a'..])
                   let vp = zip v p
                   let nbNames = map (:[]) $ take extras p
                   return ( [ FText "Give the set of isolated vertices in the graph."
                            , FGraph (JSGraph (map toFGNode vp)
                                              (map toFGEdge e))
                                     mempty
                            , FNote "Part of the graph may be outside of the visible area, make sure you zoom out a bit"
                            ]
                          , GAVertices [FText "the set of isolated vertices"] nbNames
                          )
              , do cpts <- nodes [1..3]
                   extraE <- nodes [cpts..9-cpts]
                   (v,e) <- randomGraph 10 cpts extraE
                   l <- permuteCC (take (length v) ("abcd" ++ ['f'..]))
                   let vl = zip v l
                   theEdge <- nodes e
                   let edges = [ (if e' == theEdge then labelEdge "e" else id)
                               $ toFGEdge e'
                               | e' <- e]
                   let nbNames = map (:[]) $ massNth (sort [fst theEdge, snd theEdge]) l
                   return ( [ FText "Give the set of of endpoints of ",FMath "e"
                            , FGraph (JSGraph (map toFGNode vl) edges)
                                     mempty
                            ]
                          , GAVertices [FText "the set of endpoints of ",FMath "e"] nbNames
                          )
              ] produceQuestion checkGAnswer

checkGAnswer :: (a, GraphAnswerType)
             -> Map.Map String String -> ProblemResponse -> ProblemResponse
checkGAnswer (_,GAEdges q lsts) mp pr = rosterFeedback2 (q,lsts) mp pr
checkGAnswer (_,GAVertices q lsts) mp pr = rosterFeedback (q,lsts) mp pr

produceQuestion :: ([Field], GraphAnswerType) -> Exercise -> Exercise
produceQuestion (flds,GAEdges{}) ex
  = ex{eQuestion = flds ++ [FFieldMath "roster"]}
produceQuestion (flds,GAVertices{}) ex
  = ex{eQuestion = flds ++ [FFieldMath "roster"]}

toFGEdge :: Edge -> JSEdge
toFGEdge (v1,v2)= JSEdge v1 v2 mempty

labelEdge :: String -> JSEdge -> JSEdge
labelEdge s e = e{jseOptions = (jseOptions e){jsoLabel = Just s}}

toFGNode :: (Vertex, Char) -> JSNode
toFGNode (v, c) = JSNode v mempty{jsoLabel=Just [c]}


graphStub :: ExerciseType
graphStub = exerciseType "GraphStub" "(testing)" "Displaying graphs" 
              [boolTree] genGraph unknownFeedback
{-


2a. Give the edge-set of the subgraph induced by {vertex-set}.
2b. Give the number of connected components of the subgraph induced by {vertex-set}.
2c. Give the vertices that have a degree of x in the subgraph induced by {vertex-set}.

3a. Give the set of vertices that are on some path from a to b
3b. Give the set of vertices that are on some walk from a to b
3c. Give the set of vertices that are on some cycle of G

3. Which of the following are true in graph G, in which the vertices a and b are labeled:
- vertices a and b are equal
  / the set of paths that include a is equal to the set of paths set include b
  / the set of paths starting in a is equal to the set of paths starting in b
  / the set of edges incident to a is equal to the set of edges incident to b
- the set of neighbors of vertices a and b is equal
- the degree of vertices a and b is equal
- the set of cycles that include a is equal to the set of cycles that includes b
- the connected components that include a are equal to the connected components that include b
  / the set of vertices reachable from a is equal to the set of vertices reachable from b

4. Is graph G bipartite?
- If so, give the vertices in one of its two partitions
- If not, give the vertices in an odd cycle

        -}
genGraph :: a -> Exercise -> Exercise
genGraph _ def 
 = def{ eQuestion = [ FText $"Here is an example graph"
                    , FGraph someGraph graphOptions ]
      , eBroughtBy = ["Sebastiaan Joosten"] }
 where someGraph :: JSGraph
       someGraph = JSGraph [node1,node2,node3] [edge1,edge2]
       graphOptions = JSOptions nodeOptions edgeOptions
       nodeOptions = mempty{jsoShape = Just "dot", jsoSize = Just 4}
       edgeOptions = mempty
       node1 = JSNode 0 (mempty{jsoLabel = Just "a"})
       node2 = JSNode 1 (mempty{jsoLabel = Just "b"})
       node3 = JSNode 2 (mempty{jsoLabel = Just "c"})
       edge1 = JSEdge 0 1 mempty
       edge2 = JSEdge 0 2 mempty

unknownFeedback
        :: a -> Map.Map String String
        -> ProblemResponse -> ProblemResponse
unknownFeedback _ _ pr = pr