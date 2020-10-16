-- | This module doesn't really have an exercise, it is just there to show how to draw graphs.
--   The exported exercise 'stub' does only that: draw a graph with three vertices and two edges.
module CS30.Exercises.Graphs (graphStub) where
import CS30.Data
import CS30.Exercises.Data
import Data.Map as Map

graphStub :: ExerciseType
graphStub = exerciseType "GraphStub" "(testing)" "Displaying graphs" 
              [boolTree] genGraph unknownFeedback

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