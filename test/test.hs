import           CS30.Data
import           CS30.Exercises (pages)
import           CS30.Exercises.Data
import qualified Data.Text as Text
import qualified Test.QuickCheck.Property as QCP
import Test.Tasty.HUnit
import           Test.Tasty
-- import Test.Tasty.SmallCheck as SC
import           Test.Tasty.QuickCheck as QC
import           Text.TeXMath.Readers.TeX (readTeX)
import Debug.Trace
import           CS30.Exercises.LogicExpr.Proof (checkLaw, input_laws, fake_laws)

_unused :: a
_unused = undefined where _ = trace

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [pagesStandardTests, lawTests]

lawTests :: TestTree
lawTests
 = testGroup "LogicExpr laws" ([testLaw True l | l <- input_laws] ++
                               [testLaw False l | l <- fake_laws])
 where testLaw b l
          = testCase l $ assertBool ("This law should have been "++show b ++" but was not evaluated as such.")
                                    (b == checkLaw l)

pagesStandardTests :: TestTree
pagesStandardTests
 = testGroup "StandardTests" [testPage page | page <- pages]
 where testPage et
         = testGroup (etTag et)
            [ QC.testProperty "Case tree has no dead-ends"
                (withMaxSuccess 5000 $ noDeadEnds "" (etChoices et))
            , choiceT "There must be at least 11 problems for every difficulty level (ideally 50+)"
                (once . over 10 . enumAll)
            , choiceT "Sanity checks on Field in the generated problem"
                (someTrees validFields)
            ]
         where choiceT s f
                = testGroup s [ QC.testProperty ("Problem "++show i) (f c)
                              | (c,i) <- zip (etChoices et) [(0::Int)..] ]
               validFields f = conjoin (map checkField (let ex = etGenEx et f defaultExercise
                                                        in eQuestion ex ++ eHidden ex))

checkField :: Field -> Property
checkField = property . validField

resultAll :: [QCP.Result] -> QCP.Result
resultAll = foldr preferFail QCP.succeeded
 where preferFail r1 r2
        = case QCP.ok r1 of
            Just False -> r1
            _ -> r2
validField :: Field -> QCP.Result
validField (FGraph _ _) = QCP.succeeded -- TODO: think about sanity checks some more
validField (FFieldBool _ _ _ _) = QCP.succeeded -- TODO: think about sanity checks some more
validField (FTable _o@(h:tl)) = if (all ((== length h) . length) tl) then QCP.succeeded else QCP.failed{QCP.reason = "The table is not a rectangle (different numbers of columns)."} -- TODO: recurse into table
validField (FTable []) = QCP.failed{QCP.reason = "The table is empty, empty tables should not be generated."}
validField (FFieldMath str) = validHTMLName "The Field with constructor FFieldMath" str
validField (FText str) = seq str QCP.succeeded
validField (FNote str) = seq str QCP.succeeded
validField (FIndented n lst) | n >= 0 = resultAll (map validField lst)
validField (FIndented _n _lst) = QCP.failed{QCP.reason = "Indentation cannot be negative."}
validField (FChoice nm lst)
 = resultAll (validHTMLName "The Field with constructor FValue" nm:concatMap (map validField) lst)
validField (FReorder nm lst)
 = resultAll (validHTMLName "The Field with constructor FValue" nm:concatMap (map validField) lst)
validField (FValue a b)
 = seq b (validHTMLName "The Field with constructor FValue" a)
validField (FValueS a b)
 = seq b (validHTMLName "The Field with constructor FValueS" a)
validField (FMath str)
  = case readTeX (Text.pack str) of -- TODO: check latex fields and match to MathQuill, change to a different parser that matches MathQuill's.
         Left _e -> -- trace ("The string "++str++" wasn't parsed as valid LaTeX-math.\n"++Text.unpack _e)
                   QCP.succeeded
         Right _lst -> QCP.succeeded

validHTMLName :: String -> String -> QCP.Result
validHTMLName where' str
 = QCP.rejected{QCP.ok = Just (all (flip elem (['A'..'Z']++"_"++['a'..'z']++['0'..'9'])) str)
               ,QCP.reason = where' ++" has "++show str++" as argument, but this argument needs to be a valid HTML-form name for POST data, use only characters, underscore and numbers."
               }

someTrees :: (a -> Property) -> ChoiceTree a -> Property
someTrees f (Node a) = f a
someTrees f (Branch lst) = forAllBlind (QC.elements lst) (someTrees f)

enumAll :: ChoiceTree a -> [a]
enumAll (Node a) = [a]
enumAll (Branch lst) = concatMap enumAll lst

over :: Int -> [a] -> Property
over i lst = property$ case drop i lst of
               [] -> QCP.failed{QCP.reason = "Too few elements, only "++show (length lst)}
               _ -> QCP.succeeded


ok :: Property
ok = property QCP.succeeded

noDeadEnds :: String -> [ChoiceTree a] -> Property
noDeadEnds s [] = property (QCP.failed{QCP.reason = "Empty list, path in ChoiceTree was: "++s})
noDeadEnds s lst
   = if length branches == 0
     then ok
     else forAllBlind (QC.elements branches) id
   where branches = [ noDeadEnds (s ++ "/" ++ show i) lst'
                    | (Branch lst', i) <- zip lst [(0::Int)..]]
{-

data ExerciseType
  = ExerciseType{ etTag :: String
                , etMenu :: String
                , etTitle :: String
                , etChoices :: [ChoiceTree Text.Text]
                , etGenEx :: Text.Text -> Exercise -> Exercise
                , etGenAns :: Text.Text -> Map.Map String String -> ProblemResponse -> ProblemResponse
                }
-}
    
{-   
scProps = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , SC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
  , SC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 SC.==> x^n + y^n /= (z^n :: Integer)
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , QC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
  , QC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
  ]

unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,2] @?= LT
  ]
-}