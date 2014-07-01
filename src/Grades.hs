{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction, RecursiveDo #-}
-- module GradesHS where

import	GradesTypes
import	Text.Printf
import	Data.Time
import	Data.Default (def)
import	Data.Maybe
import qualified Data.Map.Strict as Map
-- import				Control.Concurrent (threadDelay)
import 	Data.IORef	-- safe state
-- import 				Control.Monad.Trans (liftIO)
import	Control.Lens hiding (set)
import	Control.Monad (when,void,liftM)
import 	Graphics.UI.Threepenny.Core hiding (delete)
import qualified Graphics.UI.Threepenny as UI

-- | Different average calculations. NN indicates that negative should be ignored.
data LAvg = SWS | ECTS | UW | NN LAvg deriving (Eq,Ord) 		

-- | Available actions from the interface
data LAction = 	RemSub Int			-- remove subject with index 		
			|	RemRes Int			-- remove result from subject with index
			|	AddRes Int (Maybe LResult)	-- add result to index
			|	AddSub (Maybe LSubject)		-- add subject
			-- |	DoNothing			-- in case of failure

makeClassy ''LSubject
makeClassy ''LResult

maybeDouble :: String -> Maybe Double
maybeDouble = fmap fst . listToMaybe . reads

maybeDay :: String -> Maybe Day
maybeDay = fmap fst . listToMaybe . reads

gradesDouble :: Map.Map LGrade Double
gradesDouble = Map.fromList $ zip [AT S1, AT U2, AT B3, AT G4, AT N5] [1.0,2.0,3.0,4.0,5.0]

-- | Make subject to instance of status for convenience
instance Status LSubject where
	toStatus s = maybe Todo (toStatus . (^.grade)) (s^.result)

createSubject :: Double -> Double -> String -> LType -> LSubject
createSubject e s n t  = def 
	& ects	.~ e
	& sws	.~ s
	& name	.~ n
	& ltype	.~ t

createResult :: (Int,Int, Integer) -> LGrade -> String -> LResult
createResult (d,m,y) g p = def
	& date	.~ fromGregorian y m d
	& grade	.~ g
	& prof	.~ p

addResult :: LResult -> LSubject -> LSubject
addResult r s = s & result .~ Just r
	
-- | for easier creation from user input
createSubject' :: String -> String -> String -> Maybe LSubject
createSubject' e t n
	| isJust $ maybeDouble e = Just $ createSubject (read e) 0 n (fromString' t)
	| otherwise = Nothing
	
createResult' :: String -> String -> String -> Maybe LResult
createResult' d g p
	| isJust $ maybeDay d	= Just $ createResult'' (read d) (fromString' g) p
	| otherwise 			= Nothing

createResult'' :: Day -> LGrade -> String -> LResult
createResult'' d g p = def
	& date	.~ d
	& grade	.~ g
	& prof	.~ p
	
-- | Calculates the average of the provided subjects with the specified method
avg :: LAvg -> [LSubject] -> Double
avg (NN w) x = avg w [s | s <- x, toStatus s == Pos] -- in this case only avg of positive subjects will be considered
avg wi x  = sumgrades/sumweight  where
	y = filterGraded x			-- filter out ungraded subjects
	weights = case wi of		-- get weights of subjects according to wi (Weight Identifier)
		ECTS-> y^..traversed.ects
		SWS -> y^..traversed.sws
		UW	-> replicate (length y) 1.0
	getgrades z = catMaybes (z ^.. traversed.result) ^.. traversed.grade -- gets grades from subjects
	gradesdouble = map (`Map.lookup` gradesDouble) (getgrades y)	-- looks up double value of grades
	filtered = [(z,y)|(x,y)<-zip gradesdouble weights,(Just z) <- [x],isJust x] -- filters out grades without double
	sumweight = sum $ map snd filtered
	sumgrades = sum $ map (uncurry (*)) filtered
	
filterStatus :: [LStatus] -> [LSubject] -> ([LSubject] -> b) -> b
filterStatus status s f = f [x | x <- s, toStatus x `elem` status]

filterGraded :: [LSubject] -> [LSubject]
filterGraded = filter (\e -> isJust $ e ^. result)

-- | Applies an action to a list of subjects and returns the modified list.
applyAction :: LAction -> [LSubject]-> [LSubject]
applyAction (RemSub i) s 	= ys ++ tail zs where (ys,zs) = splitAt i s
applyAction (RemRes i) s 	= s & ix i . result .~ Nothing
applyAction (AddSub (Just n)) s 	= s ++ [n]
applyAction (AddRes i (Just r)) s 	= s & ix i . result .~ Just r
applyAction _ s = s
-- applyAction _ s= s
	
-- Some data to demonstrante how it works.
so = addResult (createResult (10,12,2013) (G "+") "Univ.-Prof Dr. Gerhard Krexner")(createSubject 2 1 "Sophomore" SE) 
e3 = addResult (createResult (10,12,2013) (AT S1) "Prof. iR Dr. Wolfgang Pfeiler")(createSubject 6 1 "Einfuehrung in die Physik III" VO) 
e4 = addResult (createResult (19,02,2014) (AT G4) "Prof. iR Dr. Wolfgang Pfeiler")(createSubject 6 1 "Einfuehrung in die Physik IV" VO) 
cp = addResult (createResult (26,03,2014) (AT S1) "Univ.-Prof Dr. Martin Neumann")(createSubject 5 1 "Computational Physics" VO)
cp2 = createSubject 5 1 "Computational Physics II" VO
stat = addResult (createResult (3,03,2014) (AT N5) "Univ.-Prof Dr. Reinhard Viertl") (createSubject  3 1   "Statistik und Wahrscheinlichkeitstheorie" VO)
std = map Just [cp,e3,e4,stat,so,cp2] ++ [Nothing]


-- GUI (ugly code starts here)
main :: IO ()
main = startGUI defaultConfig {tpPort = Just 9999, tpStatic = Just "./" } (setup std)
	
setup :: [Maybe LSubject] -> Window -> UI ()
setup s w = void $ do
	return w UI.# set title "Grades.HS"
	UI.addStyleSheet w "concept.css"
	io <- liftIO $ newIORef (catMaybes s)
	view	<- mkView (w,io) s
	getBody w	UI.# set UI.children [view]

mkView :: (Window,IORef [LSubject]) -> [Maybe LSubject] -> UI Element
mkView w s = do
	-- asdf <- trace "fuckthis" (UI.div)
	let header = UI.h1  #+ [string "Grades.HS"]
	UI.div #. "main" #+	[header,mkSubjects w s, mkStats s]

-- | Subject list
mkSubjects  :: (Window,IORef [LSubject]) -> [Maybe LSubject] -> UI Element
mkSubjects w s = UI.div #. "subjects" #+ (UI.h2 UI.# set text "Subjects":iterateWithIndex 0 (mkSubject w) s)

-- | Entry in subject list
mkSubject :: (Window,IORef [LSubject])-> Int -> Maybe LSubject -> UI Element
mkSubject w index subj' = do
	let subj = fromMaybe def subj'
	etitle	<- UI.div	#. "title"	#+
		[	UI.div	#.	"ects"	#+ [string . show $ subj ^. ects]
		,	UI.div	#.	"type"	#+ [string . show $ subj ^. ltype]
		,	UI.div	#.	"name"	#+ [string $ subj ^. name]
		]
	egrade	<- UI.div	#. "grade" #+ [string . show $ maybe def (^.grade) (subj ^. result)]
	eexam	<- UI.div	#. "exam"	#+
		[	UI.div	#.	"date"	#+ [string $ maybe def (show .(^.date)) (subj ^. result)]
		,	UI.div	#.	"prof"	#+ [string $ maybe def (^.prof) (subj ^. result)]	
		]
	buttons <- mkButtons w index subj'
	let items = if isJust subj' then [etitle,egrade,eexam] else []
	UI.div		#.	subjClasses subj'	UI.# set UI.children (items ++ buttons)

-- | Provides buttons and input fields for user entry/modifications. Also binds the those elements.	
mkButtons :: (Window,IORef [LSubject])-> Int -> Maybe LSubject -> UI [Element]
mkButtons (w,io) index su = do
	let isnew = isNothing su
	let graded	= isJust $ fromMaybe def su ^.result
	brem	<- UI.button#. ("sb1 " ++ show index)	#+ [string $ if isnew then "+" else "-"]
	bres	<- UI.button#. ("sb2 " ++ show index)	#+ [string $ if graded then "-" else "+"]

	-- create subject 
	e	<- UI.input #. "input iects"
			UI.# set (attr "placeholder") "ECTS"
			UI.# set (attr "pattern")"[0-9]+[.]?[0-9]?"	
	t	<- UI.input #. "input itype"
			UI.# set (attr "placeholder") "Type"
			UI.# set (attr "pattern")"(?i)VU|VO|SE|PR|LU|UE"
	n	<- UI.input #. "input iname"
			UI.# set (attr "placeholder") "Subject"
	-- create exam (result)
	d	<- UI.input #. "input2 idate"
			UI.# set (attr "placeholder") "4Y-2M-2D"
			UI.# set (attr "pattern")"[0-9]{4}[-][0-9]{2}[-][0-9]{2}"	
	p	<- UI.input #. "input2 iprof"
			UI.# set (attr "placeholder") "Examinant"
	g	<- UI.input #. "input2 igrade"	
			UI.# set (attr "placeholder")  "G"
			UI.# set (attr "pattern")"[1|2|3|4|5|+|-]"	
	
	on UI.click brem $ \_ -> do
		if isnew then do
			subj<- getSubject (e,t,n)
			liftIO $ modifyIORef io (applyAction (AddSub subj))
		else
			liftIO $ modifyIORef io (applyAction (RemSub index))
		outputNewState (w,io)

	on UI.click bres $ \_ -> do
		if graded then
			liftIO $ modifyIORef io (applyAction (RemRes index))
		else do
			res	<- getResult (d,p,g)
			liftIO $ modifyIORef io (applyAction (AddRes index res))		
		outputNewState (w,io)
	let out = arbitraryName where
		arbitraryName
			| isnew = brem :[e,t,n]
			| graded = [brem,bres]
			| otherwise = [brem,bres] ++ [d,p,g]
	return out where

-- | Renders the current content of the IORef in the specified window
outputNewState :: (Window, IORef [LSubject]) -> UI Element
outputNewState (w,io) = do
	rSubject <- liftIO $ readIORef io
	view	<- mkView (w,io) (map Just rSubject ++ [Nothing])
	getBody w	UI.# set UI.children [view]

-- | Returns result from the form elements
getResult :: (Element, Element, Element) -> UI (Maybe LResult)
getResult (di,pi,gi) = do
	d <- di UI.# UI.get value
	g <- gi UI.# UI.get value
	p <- pi UI.# UI.get value
	return $ createResult' d g p

-- | Returns subject from the form elements
getSubject :: (Element, Element, Element) -> UI (Maybe LSubject)	
getSubject (ei,ti,ni) = do
	e <- ei UI.# UI.get value
	t <- ti UI.# UI.get value
	n <- ni UI.# UI.get value
	return $ createSubject' e t n
	
-- | iterates through list and calls function with index of the element	
iterateWithIndex :: Int -> (Int -> a -> b) -> [a] -> [b]
iterateWithIndex _ _ [] = []
iterateWithIndex i f (c:cs) = f i c : iterateWithIndex (i+1) f cs

-- | String represantation of the classes of a subject
subjClasses :: Maybe LSubject -> String
subjClasses Nothing	= "subject new"
subjClasses (Just g)= "subject " ++ show (toStatus g)

-- Generats statistics
mkStats :: [Maybe LSubject] -> UI Element
mkStats s'= UI.div #. "stats" #+ [UI.h2 UI.# set text "Statistics"
			,UI.div #. "statwrap" #+ stats] where
			s = catMaybes s'
			-- emptyline = (replicate 38 '_',def)
			f1d = printf "%.1f"
			f2d = printf "%.2f"
			stats :: [UI Element]
			stats = map formatStat 
					[	("ECTS",def)
					,	("Total:", f1d . sum $ s^.. traversed . ects)
					,	("Positive:", f1d . sum $ filterStatus [Pos] s (^.. traversed . ects))
					,	("Remaining:", f1d . sum $ filterStatus [Neg,Todo] s (^.. traversed . ects))
					-- ,	emptyline
					,	("Averages",def)
					,	("ECTS:", f2d $ avg ECTS s)
					,	("ECTS (positive):", f2d $ avg (NN ECTS) s)
					,	("Unweighted:", f2d $ avg UW s)
					,	("Unweighted (positive):", f2d $ avg (NN UW) s)
					-- ,	emptyline
					,	("Subjects",def)
					,	("Total:", show . length $ s)
					,	("Positive:", show . length $ filterStatus [Pos] s id)
					,	("Negative:", show . length $ filterStatus [Neg] s id)
					]
			formatStat :: (String,String) -> UI Element
			formatStat (l,v) = UI.div #. "statline" #+ 
				[	UI.span #. ("label" ++ (if v==def then " h5" else [])) UI.# set text l
				,	UI.span #. "value" UI.# set text v
				]
