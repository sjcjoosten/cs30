{-# LANGUAGE TemplateHaskell,  OverloadedStrings #-}
-- | This module doesn't really have an exercise, it is just there to show how to draw graphs.
--   The exported exercise 'stub' does only that: draw a graph with three vertices and two edges.
module CS30.Exercises.Graphs (graphStub, graphsGiveSet) where
import           CS30.Data
import           CS30.Exercises.Data
import           CS30.Exercises.SetBasics.SolutionChecker
import           CS30.Exercises.Util
import           Data.Aeson.TH ( defaultOptions, deriveJSON )
import CS30.Exercises.Cardinality (parseExpr, evalExpr)
import           Data.List
import Data.List.Extra ( nubSort )
-- import           Debug.Trace
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy as Text
import           Data.Void
import           GHC.Stack ( HasCallStack )
import           Text.Megaparsec
import Text.Megaparsec.Char ( string )
-- TODO: code duplication, especially with SolutionChecker.hs

getNum :: String -> Either String Integer
getNum nr =
  case parse parseExpr nr "" of
    Left _e -> Left ("Could not understand a number you typed")
    Right v -> case evalExpr v of
                 Nothing -> Left ("Could not evaluate your computation")
                 Just v' -> Right v'

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
   (distribute . map (addInfo edgeSpace)) >>=
   (return . joinComponents)
 where
   distribute lst' = go [] extra_edges (sum . map snd $ lst') lst'
    where go _ _ _ [] = return []
          go acc selC remaining ((v,lim):lst)
           | lim <= 0  = (:) (addEdge v acc) <$> go [] selC remaining lst
           | otherwise = lim1 `seq` remaining `seq` selC `seq` id $
                         do draw <- (nodes [1..remaining])
                            (if draw <= selC
                              then go (lim1:acc) (selC - 1)
                              else go acc selC
                              ) (remaining - 1) ((v,lim1):lst)
               where lim1 = lim - 1

-- | As with randomGraph, but adds labels to all graph vertices
fullyLabeled :: Int -> Int -> Int -> ChoiceTree ([Vertex], [Char], [Edge])
fullyLabeled v n e
  = do (vs,es) <- randomGraph v n e
       p <- permute (take v ['a'..])
       return (vs,p,es)

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
  = GAEdges [Field] [(String,String)] -- set of edges.
  | GAVertices [Field] [String] -- set of vertices.
  | GADegree [Field] Integer
$(deriveJSON defaultOptions ''GraphAnswerType)

-- | Graph basics questions:
-- a. Give the set of neighbors of the vertex a.
-- b. Give the set of isolated vertices in graph G.
-- c. Give the set of all endpoints of the edge (u,v).
-- (Use roster notation for sets of edges or vertices, use pair notation for edges)
graphsGiveSet :: ExerciseType
graphsGiveSet = exerciseType "GraphsGiveSet" "L17" "Graph basics 1"
              -- do (v,l,e) <- fullyLabeled 10 1 7
              --    let vl = zip v l
              --    rndm <- nodes vl
              --    -- trace (show vl) (return ())
              --    let neighbors = sort ([v1 | (v1,v2) <- e, v2 == fst rndm]
              --                        ++[v2 | (v1,v2) <- e, v1 == fst rndm])
              --    let nbNames = massNth neighbors vl
              --    -- trace ("Taking: "++show neighbors ++ "\n"++show nbNames) (return ())
              --    return ( [ FText "Give the set of incident edges of the vertex "
              --             , FMath [snd rndm],FText "."
              --             , FGraph (JSGraph (map toFGNode vl) (map toFGEdge e)) mempty
              --             , FNote "You can zoom and scroll in the graph."
              --             ]
              --           , GAEdges [FText "the set of incident edges"]
              --                     [[[nm],[snd rndm]] | (_,nm) <- nbNames]
              --           )
              --
              [ do (v,l,e) <- fullyLabeled 10 3 7
                   let vl = zip v l
                   rndm <- nodes vl
                   let neighbors = sort ([v1 | (v1,v2) <- e, v2 == fst rndm]
                                       ++[v2 | (v1,v2) <- e, v1 == fst rndm])
                   let nbNames = map (:[]) $ massNth neighbors l
                   return ( [ FText "Give the set of neighbors of the vertex "
                            , FMath [snd rndm],FText ". In the graph "
                            , FMath (graphAsSet vl e)
                            , FGraph (JSGraph (map toFGNode vl) (map toFGEdge e))
                                     mempty
                            , FNote "You can zoom (by scrolling or pinching) and pan (by dragging)."
                            ]
                          , GAVertices [FText "the set of neighbors"] nbNames
                          )
              , do g <- randomGraph 7 2 3
                   extras <- nodes [2..6]
                   let (v,e) = joinComponents [([0..extras-1],[]), g]
                   p <- permute (take (length v) ['a'..])
                   let vp = zip v p
                   let nbNames = map (:[]) $ take extras p
                   return ( [ FText "Give the set of isolated vertices in the graph "
                            , FMath (graphAsSet vp e)
                            , FGraph (JSGraph (map toFGNode vp)
                                              (map toFGEdge e))
                                     mempty
                            , FNote "Part of the graph may be outside of the visible area, make sure you zoom out a bit to ensure you see all vertices or check the formal description of the graph."
                            ]
                          , GAVertices [FText "the set of isolated vertices"] nbNames
                          )
              , do cpts <- nodes [1..3]
                   extraE <- nodes [cpts..9-cpts]
                   (v,e) <- randomGraph 10 cpts extraE
                   l <- permute (take (length v) ("abcd" ++ ['f'..]))
                   let vl = zip v l
                   theEdge <- nodes e
                   let edges = [ (if e' == theEdge then labelEdge "e" else id)
                               $ toFGEdge e'
                               | e' <- e]
                   let nbNames = map (:[]) $ massNth (sort [fst theEdge, snd theEdge]) l
                   let lookupV nr = case lookup nr vl of
                                       Nothing -> error "Wrong edge"
                                       Just c -> [c]
                   return ( [ FText "Give the set of of endpoints of "
                            , FMath $ "e = (" ++ lookupV (fst theEdge) ++","++lookupV (snd theEdge)++")"
                            , FText ". In the graph "
                            , FMath (graphAsSet vl e)
                            , FGraph (JSGraph (map toFGNode vl) edges)
                                     mempty
                            ]
                          , GAVertices [FText "the set of endpoints of ",FMath "e"] nbNames
                          )
              ]  produceQuestion checkGAnswer

graphAsSet :: [(Vertex, Char)] -> [Edge] -> String
graphAsSet vts es'
  = "\\left(\\left\\{"<>intercalate "," (nubSort (map (\(_,c) -> [c]) vts))
                      <>"\\right\\}, \\left\\{"
                      <>intercalate "," (map (\(x,y) -> "("<>x<>","<>y<>")") es)
                      <>"\\right\\}\\right)"
  where findVtx nr = case lookup nr vts of
                       Nothing -> error "Invalid edge"
                       Just c -> [c]
        es = nubSort (map (\(x,y) -> sortPair (findVtx x) (findVtx y)) es')

sortPair :: Ord b => b -> b -> (b, b)
sortPair a b | a<=b = (a,b)
 | otherwise = (b,a)

checkGAnswer :: (a, GraphAnswerType)
             -> Map.Map String String -> ProblemResponse -> ProblemResponse
checkGAnswer problemDesc mp defaultRsp
  | (_,GAEdges q lsts) <- problemDesc    = edgesFeedback (q,lsts)
  | (_,GAVertices q lsts) <- problemDesc = verticesFeedback (q,lsts)
  | (_,GADegree q lsts) <- problemDesc   = degreeFeedback (q,lsts)
  where
    usr = Map.lookup "roster" mp
    wrong = markWrong defaultRsp{prTimeToRead = 10}
    correct = markCorrect defaultRsp{prTimeToRead = 5}
    usrError = tryAgain wrong{prFeedback= [FText "I didn't quite understand that, it appears to me like a miscommunication between client and server. If that didn't happen, please report this exercise and mention this message!"]}
    degreeFeedback :: ([Field], Integer) -> ProblemResponse
    degreeFeedback (quer, sol)
      = case usr of
          Nothing -> usrError
          Just v -> case getNum v of
            Left _ -> tryAgain wrong{prFeedback= [FText "I didn't quite understand that, please type a number (like '0')."]}
            Right nr -> if nr == sol then correct{prFeedback=rsp}
                        else wrong{prFeedback=rsp++[FText$ ". You answered: "++show nr]}
      where rsp = quer++[FText " is ", FMath (show sol)]
    verticesFeedback :: ([Field], [String]) -> ProblemResponse
    verticesFeedback (quer, sol)
      = case usr of
          Nothing -> usrError
          Just v -> case getSet (Text.pack v) of
                      Left _ -> tryAgain wrong{prFeedback= [FText "I didn't quite understand that, please use roster notation with a set of vertices: {a,b,c}, for instance. In Math notation:",FFieldMath "\\{a,b,c\\}"]}
                      Right st -> if nub st == st then
                                      (if Set.fromList st == Set.fromList (map Text.pack sol)
                                       then correct{prFeedback=rsp}
                                       else wrong{prFeedback=rsp++[FText$ ". You answered a different set: "]++rspwa})
                                   else tryAgain wrong{prFeedback=[FText ". Your answer contained duplicate elements"]}
      where solTeX = dispSet sol
            rsp = [FText $ "In roster notation, "]++quer++[FText " is ", FMath solTeX]
            rspwa = case usr of
                      Nothing -> [FText "- ??? - (perhaps report this as a bug?)"]
                      Just v -> [FMath v]
    -- Answer whether a user-given set matches the set in the solution for sets of sets (no sets within those, just elements)
    edgesFeedback :: ([Field], [(String,String)]) -> ProblemResponse
    edgesFeedback (quer, sol)
      = case pr of
        Nothing -> tryAgain wrong{prFeedback= [FText "I didn't quite understand that, please use roster notation with a set of edges: {(a,b),(c,d)}, for instance."]}
        Just v -> if Set.fromList (map Set.fromList v) == Set.fromList (map (\(x,y) -> Set.fromList [x,y]) sol)
                  then correct{prFeedback=rsp }
                  else wrong{prFeedback=rsp++[FText$ ". You answered a different set: "]++rspwa}
      where solTeX = dispSet (map (\(x,y)->concat ["(",x,",",y,")"]) sol)
            -- usr = Map.lookup "roster" usr'
            pr :: Maybe [[String]]
            pr = case usr of
                  Nothing -> Nothing
                  Just v -> case getSet (Text.pack v) of
                              Left _ -> Nothing
                              -- Left str -> error (errorBundlePretty str)
                              Right st -> mapM getSet2 st
            getSet2 x = case getTuple x of
                          Left _ -> Nothing
                          Right st -> Just (map Text.unpack st)
            rsp :: [Field]
            rsp = [FText $ "In roster notation, "]++quer++[FText " is ", FMath solTeX]
            rspwa = case usr of
                    Nothing -> [FText "- ??? - (perhaps report this as a bug?)"]
                    Just v -> [FMath v]

getTuple :: Text.Text -> Either (ParseErrorBundle Text.Text Void) [Text.Text]
getTuple = fmap removeQuotes . parse (parseWS *> (string "(" *> parseInnerSet <* string ")") <* parseWS) ""

produceQuestion :: ([Field], GraphAnswerType) -> Exercise -> Exercise
produceQuestion (flds,GAEdges{}) ex
  = ex{eQuestion = flds ++ [FFieldMath "roster"]}
produceQuestion (flds,GAVertices{}) ex
  = ex{eQuestion = flds ++ [FFieldMath "roster"]}
produceQuestion (flds,GADegree{}) ex
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
       graphOptions = mempty{jsoNodes = nodeOptions}
       nodeOptions = mempty{jsoShape = Just "dot", jsoSize = Just 4}
       node1 = JSNode 0 (mempty{jsoLabel = Just "a"})
       node2 = JSNode 1 (mempty{jsoLabel = Just "b"})
       node3 = JSNode 2 (mempty{jsoLabel = Just "c"})
       edge1 = JSEdge 0 1 mempty
       edge2 = JSEdge 0 2 mempty

unknownFeedback
        :: a -> Map.Map String String
        -> ProblemResponse -> ProblemResponse
unknownFeedback _ _ pr = pr