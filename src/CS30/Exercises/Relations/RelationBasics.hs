module CS30.Exercises.Relations.RelationBasics (multiplicitiesRelations) where
import CS30.Data
import CS30.Exercises.Data
import CS30.Exercises.Util
import qualified Data.Map as Map
import Control.Arrow ((***))
import Data.List ( (\\), intercalate, nub, partition )
import Data.List.Extra (nubSort)
import GHC.Exts

multiplicitiesRelations :: ExerciseType
multiplicitiesRelations
 = exerciseType "multiplicitiesRelations" "L2.1" "Multiplicities of relations"
     [do -- helper function to select a slightly larger (co)domain if the function is not supposed to be total but it turns out that it is
         let ensureTot tot image
                = if tot || (ma - mi + 1) > length image then return (mi,ma)
                else nodes [(mi-1,ma),(mi,ma+1),(mi-1,ma+1)]
                where mi = minimum image
                      ma = maximum image
         (uni,tot,sur,inj) <- (,,,) <$> boolTree <*> boolTree <*> boolTree <*> boolTree
         vals <- shrink . makeUni uni . map swap . makeUni inj <$> getUnorderedSubset [(x,y) | x<-[0..9],y<-[0..9]] 15
         let vals' = map (    (if tot then relabel (nubSort (map fst vals)) else id)
                          *** (if sur then relabel (nubSort (map snd vals)) else id)) vals
         let image = nubSort (map fst vals')
         let range = nubSort (map snd vals')
         (dom_start, dom_end) <- ensureTot tot image
         (cod_start, cod_end) <- ensureTot sur range
         let row str = [Cell (FText str), Cell (FFieldBool (FText "Yes") (FText "No") Nothing str)]
         if (image == [dom_start..dom_end]) == tot then return () else error ("Totality violation: "++show tot++show image++show (dom_start,dom_end))
         if (range == [cod_start..cod_end]) == sur then return () else error ("Surjectivity violation"++show sur++show image++show (dom_start,dom_end))
         if (nub (map fst vals') == map fst vals') == uni then return () else error "Univalence violation"
         if (nub (map snd vals') == map snd vals') == inj then return () else error "Injectivity violation"
         let orlst [] lst = lst
             orlst lst _ = lst
         rows <- map ([ row "Univalent"
                      , row "Total"
                      , row "Injective"
                      , row "Surjective"] !!) <$> permutations 4
         return ( [ FText "Consider the relation "
                  , FMath "R = ", showset show vals'
                  , FText " on domain ", showRange dom_start dom_end
                  , FText " and codomain ", showRange cod_start cod_end
                  , FText ". What are the multiplicities of "
                  , FMath "R", FText "?"
                  , FTable ([Header (FText "Multiplicity"), Header (FText "Your answer")]
                            :rows)
                  ]
                , ( [ FMath "R = ", showset show vals'
                    , FText " on domain ", showRange dom_start dom_end
                    , FText " and codomain ", showRange cod_start cod_end]
                  , [ ("Univalent", (uni, concat [ [ FMath "R", FText " is not Univalent because "
                                                   , FText (show (fst p1)++" occurs as first element in the pairs "++show p1++" and "++show p2)]
                                                 | (p1:p2:_) <- [findDups vals']]
                                          `orlst`
                                          [ FMath "R", FText " is Univalent because "
                                          , FText "Each of "
                                          , showset show image
                                          , FText " is mapped to a unique element"
                                          ]
                                          ))
                    , ("Total",     (tot, concat [ [FMath "R", FText " is not Total because "
                                                   , FText$ (show t1)++" is in the domain but does not occur as first element in ",FMath "R"]
                                                 | t1 <- take 1 ([dom_start .. dom_end] \\ map fst vals')]
                                          `orlst`
                                          [ FMath "R", FText " is Total because "
                                          , FText "each of the elements in its domain "
                                          , showRange dom_start dom_end
                                          , FText " is mapped to one or more elements"
                                          ]
                                    ))
                    , ("Surjective",(sur, concat [ [FMath "R", FText " is not Surjective because "
                                                   , FText$ (show t1)++" is in the codomain but does not occur as second element in ",FMath "R"]
                                                 | t1 <- take 1 ([cod_start .. cod_end] \\ map snd vals')]
                                          `orlst`
                                          [ FMath "R", FText " is Surjective because each of the elements in its codomain "
                                          , showRange cod_start cod_end
                                          , FText " is mapped to by one or more elements"
                                          ]))
                    , ("Injective", (inj, concat [ [FMath "R", FText " is not Injective because "
                                                   ,FText (show (fst p1)++" occurs as second element in the pairs "++show (swap p1)++" and "++show (swap p2))]
                                                 | (p1:p2:_) <- [findDups (map swap vals')]]
                                          `orlst`
                                          [ FMath "R", FText " is Injective because each of "
                                          , showset show range
                                          , FText " is mapped to from some unique element"
                                          ]))
                    ]
                  )
                )
         ]
     genericQuer
     boolAnswers

boolAnswers :: (a, ([Field],[(String, (Bool,[Field]))]))
            -> Map.Map String String
            -> ProblemResponse
            -> ProblemResponse
boolAnswers (_,(expl,mp)) user_mp pr
  = (case feedback of
      [] -> markCorrect pr{prFeedback=[FText ("All "++show (length mpl)++" parts were answered correct")]}
      v -> markWrong pr{prFeedback=[FIndented 0 expl]++v})
      {prTimeToRead = 4 + 8 * length feedback}
  where mpl = toList mp
        feedback = foldr checkAnswer [] mpl
        checkAnswer :: (String, (Bool, [Field])) -> [Field] -> [Field]
        checkAnswer (v,(a,when_err)) c
          = c ++ case Map.lookup v user_mp of
                    Nothing -> [FIndented 0 [FText$ "Your answer to "++v++" is missing."]]
                    Just "T" -> if a then [] else [FIndented 0 when_err]
                    Just "F" -> if a then [FIndented 0 when_err] else []
                    _ -> when_err

showRange :: Int -> Int -> Field
showRange start end | start + 2 < end 
 = FMath $ "\\left\\{"++show start ++",\\ldots{},"++show end++"\\right\\}"
 | otherwise = showset show [start .. end]
showset :: (a -> String) -> [a] -> Field
showset f lst = FMath $"\\left\\{"++(intercalate "," (map f lst))++"\\right\\}"
shrink :: (Eq b, Eq a) => [(b, a)] -> [(b, a)]
shrink lst
 = let dps1 = findDups lst
       dps2 = findDups (map swap lst)
       dps = take 6 $ nub (take (max 3 (6 - length dps2)) dps1++map swap dps2)
   in dps ++ take (6 - length dps) (filter (flip notElem dps) lst)
findDups :: Eq a => [(a, b)] -> [(a, b)]
findDups [] = []
findDups ((a,b):xs)
 = case partition (\(x,_) -> x == a) xs of
     ([],xs') -> findDups xs' -- reuse of xs' makes xs linear
     (lst,rm) -> (a,b):(lst ++ findDups rm)
relabel :: [Int] -> Int -> Int
relabel lkp n = head [i | (n',i) <- zip lkp [head lkp..], n==n']

makeUni :: Eq a => Bool -> [(a, b)] -> [(a, b)]
makeUni _ [] = []
makeUni True ((a,b):xs) = (a,b):makeUni True (filter (\(x,_) -> x /= a) xs)
makeUni False xs = xs

swap :: (b, a) -> (a, b)
swap (a,b) = (b,a)