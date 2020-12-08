{-# LANGUAGE TupleSections #-}
module CS30.Pages where
import           CS30.Data
import           CS30.Exercises ( pages )
import           CS30.Exercises.Data
import           CS30.Util (err500)
import           Control.Monad.Trans.State.Lazy ( put, get )
import           Data.Aeson as JSON ( decode, Value )
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Map as Map
import           Data.Maybe
import Control.Monad.IO.Class

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
getHandler _ et usr lp
 = do let pr = etGenAns et (lpPuzzelStored lp) usr defaultProblemResponse
      return (ProblemResolve lp usr (prOutcome pr),mempty{rSplash = Just (SplashPR pr)})

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
       handleStale [] = err500 ("The exercise id "++show eid++" is was not found in your profile")
       increaseLevel ld
        = let cl = ldCurrentLevel ld
              drop' = drop cl
              nl = nextUp (drop' $ ldStreakPerLevel ld)
                          (drop' $ ldWrongPerLevel ld)
                          (drop' $ ldRightPerLevel ld)
          in updateLevel ld{ldCurrentLevel = cl + nl}
       resetLevel ld = updateLevel ld{ldCurrentLevel = nextUpStrk (ldStreakPerLevel ld)}
       updateLevel ld = ld{ ldOpenProblem = Nothing }
       nextUp strk wrongs rights
        = if head0 rights > 3 * head0 wrongs then 1 + nextUpStrk (drop 1 strk) else nextUpStrk strk
       nextUpStrk (c:lst) | c >= 10 = 1 + nextUpStrk lst
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
                liftIO$ rst 1
                return mempty{rDone = True, rSplash = Just (SplashDone (etTotal etp))}
        (_,rst) -> do exc <- paintExercise etp
                      let levData = Map.lookup str (sdLevelData st)
                      return mempty{ rExercises = [exc]
                                   , rProgress = (Just . (,etTotal etp) $ maybe 0 ldCurrentStreak levData) <* rst
                                   , rDone = maybe False ldDone levData}
populateEx _ _ = return mempty

checkDone :: Rsp -> SesIO Rsp
checkDone = error "not implemented"

mkPage :: ExerciseType -> Page
mkPage etp = Page (etTag etp) (etMenu etp ++ " - " ++ etTitle etp)

-- | Select a puzzle (either the most recent or a randomly selected one),
-- | return the selected puzzle and its ID.
-- | This function is also where the automated level selection comes in
smartSelect :: String
            -> [ChoiceTree Value]
            -> SesIO (Int, Value)
smartSelect nm lst
  = do ses <- get
       let ld = fromMaybe (defaultLevelData (length lst)) $ Map.lookup nm (sdLevelData ses)
       case ldOpenProblem ld of
         Nothing -> do let lvl = ldCurrentLevel ld
                       let maxL = length lst - 1
                       (genInfo,puzzle)
                         <- if lvl > maxL
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
                       -- store the problem:
                       let ld' = Map.insert nm ld{ldOpenProblem = Just newProblem} (sdLevelData ses)
                       put (ses{sdLevelData = ld'})
                       return (nextNr, puzzle)
         Just v -> return (lpPuzzelID v, lpPuzzelStored v)

paintExercise :: ExerciseType -> SesIO Exercise
paintExercise et
 = do (eid, ex) <- smartSelect (etTag et) (etChoices et)
      let dex = defaultExercise{ eTopic = etTitle et, eActions = [Check]
                               , eHidden = [ FValueS "tag" "ExerciseType"
                                           , FValue  "exId" eid
                                           , FValueS "exTag" (etTag et) ]}
      return (etGenEx et ex dex)