{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction, RecursiveDo #-}
-- module GradesHS where

import				Control.Lens hiding (set)
import				Control.Monad (when,void)

import				Data.Maybe
import				Data.Time
import				Data.Default
import				Data.Char
import qualified	Data.Map.Strict as Map
-- import Data.Set (Set, member, empty, insert, delete)

import				Text.Printf
import				Control.Concurrent (threadDelay)
import qualified	Graphics.UI.Threepenny as UI

import 				Data.IORef
import 				Control.Monad.Trans (liftIO)

import 				Graphics.UI.Threepenny.Core hiding (delete)
import qualified	GHC.Read as R
import qualified	Text.Read.Lex as L
import				Text.ParserCombinators.ReadPrec
import qualified	Text.ParserCombinators.ReadP as P

import Debug.Trace

import Text.ParserCombinators.ReadP
  ( ReadS
  , readP_to_S
  )

data LGAustrian = S1 | U2 | B3 | G4 | N5 deriving (Eq,Ord)
data LGrade = AT LGAustrian | G String deriving (Eq,Ord)
data LType = VO | VU | UE | PR | SE | T String



data LAvg = SWS | ECTS | UW | NN LAvg deriving (Eq,Ord) 		-- Different average calculations
data LStatus = Todo | Pos | Neg deriving (Eq)					-- Different subject stati
data LAction = 	RemSub Int	-- remove subject with index 		--Available actions from the interface
			|	RemRes Int	-- remove result from subject with index
			|	AddRes Int LResult	-- add result to index
			|	AddSub LSubject		-- add subject
			|	DoNothing			-- in case of failure
-- Stores the subjects added so far.
data LState = LState 
	{ _subjects	:: [LSubject]
	} 

-- 1 Subject. Curriculum is made of this thing. Added grades to do stuff.
data LSubject = Subject
	{ _ects 	:: Double			-- Credits
	, _sws		:: Double			-- Hours/Week
	, _name		:: String			-- "Einführung in die Physik 3"
	, _ltype 	:: LType
	, _result	:: Maybe LResult	-- last try 
	} 

-- Result of an exam/laboratory/..
data LResult = Result
	{ _date		:: Day		-- Date in the system
	, _grade	:: LGrade	-- Passed with grade
	, _prof		:: String	-- Examinant
	}
makeClassy ''LResult
makeClassy ''LSubject
makeLenses ''LState


instance Default LState where
	def = LState []
instance Default LResult where
	def = Result (fromGregorian 0 0 0) (G "") ""
instance Default LSubject where
	def = Subject 0 0 "" (T "") Nothing
instance Default LGrade where
	def = G ""



class  Status a  where
	toStatus :: a -> LStatus
instance Status LSubject where
	toStatus s = maybe Todo (toStatus . (^.grade)) (s^.result)
	
instance Status LGrade where
	toStatus (G "") = Todo
	toStatus (G _)	= Pos
	toStatus (AT g) = toStatus g

instance Status LGAustrian where
	toStatus N5 = Neg
	toStatus _ 	= Pos
	

instance Show LGrade where
	show (G s)	= s
	show (AT x)	= show x

instance Show LStatus where
	show Pos = "pos"
	show Neg = "neg"
	show Todo= "todo"

instance Show LGAustrian where
	show S1		= "1"
	show U2		= "2"
	show B3		= "3"
	show G4		= "4"
	show N5		= "5"

instance Show LType where
	show (T s)	= s
	show SE		= "SE"
	show UE		= "UE"
	show VO		= "VO"
	show VU		= "VU"
	show PR		= "PR"

instance Show LSubject where
	show s = fillTo 5 (show (s^.ltype)) ++ "| " ++ fillTo 10 (s^.name) ++ "\t(" ++ show (s^.ects) ++ ")"
		++  maybe "" ((":\t" ++) . show) (s^.result) where
		fillTo :: Int -> String -> String
		fillTo n s = (take n s) ++ take (n-(length s)) (repeat ' ')

instance Show LResult where
	show r = "   " ++ (show (r^.grade)) ++ ", "  ++ (show (r^.date)) ++ " " ++ (r^.prof)

instance Read LGrade where
  readPrec =
    R.parens
    ( do L.Ident s <- R.lexP
         case s of
			"G1"-> return (AT S1)
			"G2"-> return (AT U2)
			"G3"-> return (AT B3)
			"G4"-> return (AT G4)
			"G5"-> return (AT N5)
			_	-> return (G (tail s))
    )
  readListPrec = R.readListPrecDefault 
  readList     = R.readListDefault
  


fillTo :: Int -> String -> String
fillTo n s = (take n s) ++ take (n-(length s)) (repeat ' ')


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
	
createResult' :: (String,String,String) -> String -> String -> LResult
createResult' (d,m,y) g p = createResult (read d, read m,read y) (read ('G' :g)) p

gradesDouble :: Map.Map LGrade Double
gradesDouble = Map.fromList $ zip [AT S1, AT U2, AT B3, AT G4, AT N5] [1.0,2.0,3.0,4.0,5.0]

avg :: LAvg -> [LSubject] -> Double
avg (NN w) x = avg w [s | s <- x, toStatus s == Pos] -- in this case only avg of positive subjects will be considered
avg wi x  = sumgrades/sumweight  where
	y = filterGraded x			-- filter out ungraded subjects
	weights = case wi of		-- get weights of subjects according to wi (Weight Identifier)
		ECTS-> y^..traversed.ects
		SWS -> y^..traversed.sws
		UW	-> replicate (length y) 1.0
	getgrades z = (catMaybes $ (z ^.. traversed.result))^.. traversed.grade -- gets grades from subjects
	gradesdouble = map (\x -> Map.lookup x gradesDouble) (getgrades y)	-- looks up double value of grades
	filtered = [(z,y)|(x,y)<-zip gradesdouble weights,(Just z) <- [x],isJust x] -- filters out grades without double
	sumweight = sum $ map (\(_,y) -> y) filtered
	sumgrades = sum $ map (\(x,y) -> x*y) filtered
	
filterStatus :: [LStatus] -> [LSubject] -> ([LSubject] -> b) -> b
filterStatus status s f = f [x | x <- s, elem (toStatus x) status]


filterGraded :: [LSubject] -> [LSubject]
filterGraded x = filter (\e -> isJust $ e ^. result) x


addResult :: LResult -> LSubject -> LSubject
addResult r s = s & result .~ Just r

applyAction :: LAction -> [LSubject]-> [LSubject]
applyAction (RemSub i) s 	= ys ++ (tail zs) where (ys,zs) = splitAt i s
applyAction (RemRes i) s 	= s & ix i . result .~ Nothing
applyAction (AddSub n) s 	= s ++ [n]
applyAction (AddRes i r) s 	= s & ix i . result .~ Just r
applyAction _ s= s
	
-- Testdaten
so = addResult (createResult (10,12,2013) (G "+") "Univ.-Prof Dr. Gerhard Krexner")(createSubject 2 1 "Sophomore" SE) 
e3 = addResult (createResult (10,12,2013) (AT S1) "Prof. iR Dr. Wolfgang Pfeiler")(createSubject 6 1 "Einfuehrung in die Physik III" VO) 
e4 = addResult (createResult (19,02,2014) (AT G4) "Prof. iR Dr. Wolfgang Pfeiler")(createSubject 6 1 "Einfuehrung in die Physik IV" VO) 
cp = addResult (createResult (26,03,2014) (AT S1) "Univ.-Prof Dr. Martin Neumann")(createSubject 5 1 "Computational Physics" VO)
cp2 = (createSubject 5 1 "Computational Physics II" VO)
stat = addResult (createResult (3,03,2014) (AT N5) "Univ.-Prof Dr. Reinhard Viertl") (createSubject  3 1   "Statistik und Wahrscheinlichkeitstheorie" VO)
std = [cp,e3,e4,stat,so,cp2]


-- GUI (ugly code starts here)
main :: IO ()
main = do
	startGUI defaultConfig {tpPort = Just 9999, tpStatic = Just "./" } (setup std)
	putStrLn "Ready"
	
setup :: [LSubject] -> Window -> UI ()
setup s w = void $ do
	return w UI.# set title "Grades.HS"
	UI.addStyleSheet w "concept.css"
	
	ioSubject <- liftIO $ newIORef s
	(mv,buttons) <- mkView s
	brem <- fst $ buttons !! 0
	bres <- snd $ buttons !! 0
	on UI.click brem $ \_ -> do
		liftIO $ modifyIORef ioSubject (applyAction (RemSub 0))
		rSubject <- liftIO $ readIORef ioSubject
		(mv,buttons) <- mkView rSubject
		getBody w	UI.# set UI.children mv
		getBody w	#+ [return brem, return bres]
	on UI.click bres $ \_ -> do
		liftIO $ modifyIORef ioSubject (applyAction (RemRes 0))
		rSubject <- liftIO $ readIORef ioSubject
		(mv,buttons) <- mkView rSubject
		getBody w	UI.# set UI.children mv
		getBody w	#+ [return brem, return bres]
	getBody w	#+ (map (UI.element) mv)
	-- getBody w #+ 

header :: UI Element
header = UI.h1  #+ [string "Grades.HS"]

mkView :: [LSubject] -> UI ([Element], [(UI Element, UI Element)])
mkView s = do
	-- xxxx <- trace "FUCKTHIS" (UI.div)
	(subjs,buttons) <- mkSubjects s
	m <- UI.div #. "main"
	UI.element m #+	[header,return subjs, mkStats s]
	return ([m],buttons)

-- Generates subject list
mkSubjects  :: [LSubject] -> UI (Element, [(UI Element, UI Element)])
mkSubjects s = do
	out <- UI.div #. "subjects"
	let (z,buttons) = mkS s
	let subjects = [UI.h2 UI.# set text "Subjects"] ++ z
	UI.element out #+ subjects
	return (out,buttons)

mkS :: [LSubject] -> ([UI Element], [(UI Element, UI Element)])
mkS sub = (out,y) where
	x = map mkSubject sub
	y = map mkButtons sub
	out = map (\(a,(b,c)) -> a #+ [b,c]) (zip x y)
		
mkSubject :: LSubject -> UI Element
mkSubject subj = do
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
	outer <- UI.div		#. (subjClasses subj) UI.# set UI.children [etitle,egrade,eexam]
	return outer
				
mkButtons :: LSubject -> (UI Element, UI Element)
mkButtons su = (brem,bres) where
	graded	= isJust $ su^.result
	brem	= UI.button#. "sb1"	#+ [string "-"]
	bres	= UI.button#. "sb2"	#+ [string $ if graded then "-" else "+"]
	-- return (brem,bres)
		
-- iterateWithIndex :: Int -> (Int -> a -> b) -> [a] -> [b]
-- iterateWithIndex _ _ [] = []
-- iterateWithIndex i f (c:cs) = f i c : (iterateWithIndex (i+1) f cs)

subjClasses :: LSubject -> String
subjClasses g = "subject " ++ show (toStatus g)


		
-- Generats statistics
mkStats :: [LSubject] -> UI Element
mkStats s = UI.div #. "stats" #+
		(	UI.h2 UI.# set text "Statistics"
		: 	UI.div #. "statwrap" #+ stats :[]) where
			emptyline = 	(replicate 38 '_',def)
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
					,	("Positive:", show . length $ filterStatus [Pos] s (id))
					,	("Negative:", show . length $ filterStatus [Neg] s (id))
					]
			formatStat :: (String,String) -> UI Element
			formatStat (l,v) = UI.div #. "statline" #+ 
				[	UI.span #. ("label" ++ (if v==def then " h5" else [])) UI.# set text l
				,	UI.span #. "value" UI.# set text v
				]
