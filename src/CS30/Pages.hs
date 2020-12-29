{-# LANGUAGE TupleSections #-}
module CS30.Pages (pages, handleExResponse, populateEx, mkPage, reconstructExercise) where
import           CS30.Data
import           CS30.Exercises ( pages )
import           CS30.Exercises.Data
import           CS30.Util (err500)
import           Control.Monad ( join )
import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.Trans.State.Lazy ( put, get )
import           Data.Aeson as JSON ( decode, Value )
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.List.Extra (minimumOn)
import qualified Data.Map as Map
import           Data.Maybe
-- import           Data.Ratio




reconstructExercise
        :: String -- exercise tag
        -> [Int] -- way it was generated
        -> Value -- generated value
        -> Maybe ProblemResolve -- has user's answers if there were any
        -> Exercise
reconstructExercise tag gen vl pr'
  | Just exTp <- pageLookup tag
  = case newOrRegenerate exTp gen vl defaultExercise of
      Just ex -> dumbDown ex
      Nothing -> defaultExercise{eQuestion =
                   [ FIndented 0 [FText "Could not reconstruct exercise ", FText tag]
                   , FIndented 1 [FText "Generated as: ", FText (show gen)]
                   , FIndented 1 [FText "Value: ", FText (show vl)]
                   , FIndented 1 [FText "Most likely cause is that the exercise has changed on the server side"]
                   ] ++ prFlds}
  | otherwise
  = defaultExercise{eActions = []
      ,eQuestion = [ FIndented 0 [FText "Tag no longer an exercise: ", FText tag]
                   , FIndented 1 [FText "Generated as: ", FText (show gen)]
                   , FIndented 1 [FText "Value: ", FText (show vl)]
                   ] ++ prFlds}
  where (prFlds,comments)
           = case pr' of
               Nothing -> ([],"")
               Just pr -> ( [FText "Response:", FTable$ [[Header (FText "Key"),Header (FText "Value")]] ++
                             [[Cell (FText k),Cell (FText v)] | (k,v) <- Map.toList (lrUserAnswer pr) ]
                            ]
                          , " ("++show (lrScore pr)++")")
        dumbDown ex -- remove actions and add user's answers if any
         = ex{ eActions = []
             , eTopic = eTopic ex ++ comments
             , eQuestion = [ FIndented 1 (eQuestion ex) ] ++ prFlds}

newOrRegenerate :: ExerciseType -> [Int] -> Value -> Exercise -> Maybe Exercise
newOrRegenerate et gen vl ex
  = case etGenEx et vl ex of
      Nothing -> join (etGenEx et <$> regenerate gen (Branch (etChoices et)) <*> return ex)
      x -> x
regenerate :: [Int] -> ChoiceTree a -> Maybe a
regenerate _ (Node v) = Just v
regenerate (i:is) (Branch cs)
 = case drop i cs of
    [] -> Nothing
    (x:_) -> regenerate is x
regenerate _ _ = Nothing

pageLookup :: String -> Maybe ExerciseType
pageLookup = flip Map.lookup $ Map.fromListWithKey (\k -> error ("Double keys: "++k)) (map (\et -> (etTag et,et)) pages)

-- | Handle a response from an exercise
handleExResponse :: String -> SesIO Rsp
handleExResponse s
 = case decode (L8.pack s) of
     Nothing -> err500 $ "Decoding error: "++s
     Just (v@ExResponse{})
       -> case pageLookup (exTag v) of
            Nothing -> err500 $ "The exercise-type you just made ("++exTag v++") appears to have been removed or renamed - it is no longer recognised by the server"
            Just et -> do let handler = getHandler (cAction v) et
                          exerciseResponse et (exId v) (cValue v) handler (fmap snd . handler (cValue v) . lrProblem)

getHandler
    :: Action
    -> ExerciseType
    -> Map.Map String String
    -> LevelProblem
    -> SesIO (ProblemResolve, Rsp)
getHandler Check et usr lp
 = case etGenAns et (lpPuzzelStored lp) usr defaultProblemResponse of
      Just pr -> return (ProblemResolve lp usr (prOutcome pr), mempty{rSplash = Just (SplashPR pr)})
      Nothing -> return (ProblemResolve lp usr POTryAgain, mempty{rSplash = Just (SplashFeedback "Your exercise could not be checked. Perhaps it was changed recently on the server side. If this keeps happening, please report it to your teacher.")})
-- to handle other forms: ignore the response and don't show a splash screen..
getHandler _ _ usr lp
 = return (ProblemResolve lp usr POTryAgain, mempty)

exerciseResponse :: ExerciseType
                 -> Int -- ^ The unique integer assigned to this exercise
                 -> Map.Map String String
                 -> (Map.Map String String -> LevelProblem -> SesIO (ProblemResolve, b))
                 -> (ProblemResolve -> SesIO b)
                 -> SesIO b
exerciseResponse et eid usrData handleCur handleStale'
 = do let nm = etTag et
      ses <- get
      let lDat = sdLevelData ses
      case Map.lookup nm lDat of
        Just ld -> case ldOpenProblem ld of
                     Just lp -> if lpPuzzelID lp == eid then
                                  do (reslv,rsp) <- handleCur usrData lp
                                     let newD = (case lrScore reslv of
                                                  POCorrect -> 
                                                    increaseLevel
                                                    ld{ ldStreakPerLevel = udNth ((+) 1) (lpLevel lp) (ldStreakPerLevel ld)
                                                      , ldRightPerLevel = udNth ((+) 1) (lpLevel lp) (ldRightPerLevel ld)
                                                      , ldCurrentStreak = ldCurrentStreak ld + 1
                                                      }
                                                  POIncorrect -> 
                                                    resetLevel
                                                    ld{ ldStreakPerLevel = udNth (const 0) (lpLevel lp) (ldStreakPerLevel ld)
                                                      , ldWrongPerLevel = udNth ((+) 1) (lpLevel lp) (ldWrongPerLevel ld)
                                                      , ldCurrentStreak = 0
                                                      }
                                                  POTryAgain -> ld
                                                ){ldPastProblems = reslv{lrUserAnswer = usrData}:ldPastProblems ld}
                                     put ses{sdLevelData=Map.insert nm newD lDat}
                                     return rsp
                  -- we still try to handle the (probably already completed) problem properly, but it won't be scored
                                else handleStale (ldPastProblems ld)
                     Nothing -> handleStale (ldPastProblems ld)
        Nothing -> handleStale []
 where handleStale (a:as) = if lpPuzzelID (lrProblem a) == eid then handleStale' a
                            else handleStale as
       handleStale [] = err500 ("The exercise id "++show eid++" is was not found")
       increaseLevel ld
        = let cl = ldCurrentLevel ld
              drop' = drop cl
              nl = nextUp (drop' $ ldStreakPerLevel ld)
                          (drop' $ ldWrongPerLevel ld)
                          (drop' $ ldRightPerLevel ld)
          in updateLevel ld (cl + nl)
       resetLevel ld = updateLevel ld (nextUpStrk (ldStreakPerLevel ld))
       updateLevel ld sc
        | sc >= length (etChoices et) = ld{ ldOpenProblem = Nothing, ldCurrentLevel = hardest ld }
        | otherwise = ld{ ldOpenProblem = Nothing, ldCurrentLevel = sc }
       hardest ld = fst . minimumOn snd $ [0..] `zip` ldStreakPerLevel ld
       -- zipFill0 (a:as) (b:bs) = (a,b):zipFill0 as bs
       -- zipFill0 as [] = zip as (repeat 0)
       -- zipFill0 [] bs = zip (repeat 0) bs
       nextUp strk wrongs rights
        = if head0 rights > 3 * head0 wrongs then 1 + nextUpStrk (drop 1 strk)
          else nextUpStrk strk
       nextUpStrk (c:lst) | c >= etTotal et `div` length (etChoices et) = 1 + nextUpStrk lst
       nextUpStrk _ = 0
       udNth f n lst | n > 0 = head0 lst : udNth f (n - 1) (drop 1 lst)
        | otherwise = f (head0 lst):drop 1 lst
       head0 lst = head (lst ++ [0])

-- | Populate the response with exercises based on the requested page. If there is no page, an empty list is returned (painted as a listing by JavaScript)
populateEx :: RunEnv -> Maybe String -> SesIO Rsp
populateEx re (Just ('?':str)) = populateEx re (Just str)
populateEx re (Just str@(_:_))
 | Just etp <- pageLookup str
 = do st <- get
      case (Map.lookup str (sdLevelData st), reSetScore re) of
        (Just v,Just rst) | ldCurrentStreak v >= etTotal etp && ldDone v == False
          -> -- handle the case that the user is done:
             do let sesD = Map.insert str (v{ldDone = True}) (sdLevelData st)
                put (st{sdLevelData = sesD})
                let lastId = case ldPastProblems v of
                               [] -> 0
                               (h:_) -> lpPuzzelID (lrProblem h) + 1
                rst (str, lastId - ldCurrentStreak v, lastId) (Just 1)
                return mempty{rDone = True, rSplash = Just (SplashDone (etTotal etp))}
        (levData,rst) -> do exc <- paintExercise (maybe defaultExercise (const defaultExercise{eActions=[Check,Report]}) rst) etp
                            return mempty{ rExercises = [exc]
                                         , rProgress = (Just . (,etTotal etp) $ maybe 0 ldCurrentStreak levData) <* rst
                                         , rDone = maybe False ldDone levData}
populateEx _ _ = return mempty

mkPage :: ExerciseType -> Page
mkPage etp = Page (etTag etp) (etMenu etp ++ " - " ++ etTitle etp)

-- | Select a puzzle (either the most recent or a randomly selected one),
-- | return the selected puzzle and its ID.
-- | This function is also where the automated level selection comes in
paintExercise :: Exercise -> ExerciseType 
            -> SesIO Exercise
paintExercise def et -- nm lst
  = do ses <- get
       let ld = fromMaybe (defaultLevelData (length lst)) $ Map.lookup nm (sdLevelData ses)
       case ldOpenProblem ld of
         Just v | Just ex <- etGenEx et (lpPuzzelStored v) def{ eTopic = etTitle et }
           -> return ex{ eHidden = eHidden ex ++ hiddenFs (lpPuzzelID v)}
         _ -> do (nr,newProblem) <- selectNew ld lst
                 -- store the problem:
                 let ld' = Map.insert nm ld{ldOpenProblem = Just newProblem} (sdLevelData ses)
                 put (ses{sdLevelData = ld'})
                 case etGenEx et (lpPuzzelStored newProblem)
                              def{ eTopic = etTitle et, eHidden = hiddenFs nr } of
                   Nothing -> error "Could not generate exercise based on a freshly found value"
                   Just ex -> return ex
  where nm = etTag et
        lst = etChoices et
        hiddenFs i = [ FValueS "tag" "ExerciseType"
                     , FValue  "exId" i
                     , FValueS "exTag" nm ]

selectNew :: MonadIO m
          => LevelData -> [ChoiceTree Value] -> m (Int, LevelProblem)
selectNew ld lst
 = do let lvl = ldCurrentLevel ld
      let maxL = length lst - 1
      (genInfo,puzzle) <- if lvl > maxL
                          then randomSelect (Branch lst)
                          else first (lvl:) <$> randomSelect (lst !! lvl)
      let nextNr = case ldPastProblems ld of
                     (a:_) -> 1+lpPuzzelID (lrProblem a)
                     _ -> 0
      let newProblem = LevelProblem { lpPuzzelID=nextNr
                                    , lpLevel=head genInfo
                                    , lpPuzzelGen=tail genInfo
                                    , lpPuzzelStored=puzzle
                                    }
      return (nextNr, newProblem)

      