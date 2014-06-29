{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction, RecursiveDo #-}
-- module GradesHS where

import Control.Lens hiding (set)
import Control.Monad (when,void)

import Data.Maybe
import Data.Time
import Data.Default
import Data.Char
import qualified Data.Map.Strict as Map
-- import Data.Set (Set, member, empty, insert, delete)


import Control.Concurrent (threadDelay)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (delete)




-- Different average calculations
data LAvg = SWS | ECTS | UW deriving (Eq,Ord)

-- Pure data type for representing the game state
data LGAustrian = S1 | U2 | B3 | G4 | N5 deriving (Eq,Ord)
-- instance Grade LGradeAustrian
data LStatus = Todo | Pos | Neg deriving (Eq)

data LGrade = AT LGAustrian | G String deriving (Eq,Ord)

toStatus :: LGrade -> LStatus
toStatus (G "") = Todo
toStatus (G _)	= Pos
toStatus (AT g) = toStatusAT g

toStatusAT :: LGAustrian -> LStatus
toStatusAT N5	= Neg
toStatusAT _	= Pos

data LType = VO | VU | UE | PR | SE | T String

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

instance Default LState where
	def = LState []
instance Default LResult where
	def = Result (fromGregorian 0 0 0) (G "") ""
instance Default LSubject where
	def = Subject 0 0 "" (T "") Nothing
instance Default LGrade where
	def = G ""

makeClassy ''LResult
makeClassy ''LSubject
makeLenses ''LState

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

gradesDouble :: Map.Map LGrade Double
gradesDouble = Map.fromList $ zip [AT S1, AT U2, AT B3, AT G4, AT N5] [1.0,2.0,3.0,4.0,5.0]

avg :: LAvg -> [LSubject] -> Double
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




filterGraded :: [LSubject] -> [LSubject]
filterGraded x = filter (\e -> isJust $ e ^. result) x


addResult :: LResult -> LSubject -> LSubject
addResult r s = s & result .~ Just r
	
-- User input etc.
promptLine :: String -> IO String
promptLine prompt = do
    putStr $ prompt
    getLine

promptType :: IO LType
promptType = do
	tString <- promptLine "Type (VO|VU|SE|UE|PR): "
	case (map toUpper tString) of
		"VO"-> return VO
		"VU"-> return VU
		"SE"-> return SE
		"UE"-> return UE
		"PR"-> return PR
		_	-> promptType
		-- _	-> return $T tString
		

promptGrade :: IO LGrade
promptGrade = do
	gString <- promptLine "Grade (1,2,3,4,5, [..]): "
	case gString of
		"1" -> return (AT S1)
		"2" -> return (AT U2)
		"3" -> return (AT B3)
		"4" -> return (AT G4)
		"5" -> return (AT N5)
		_ 	-> return $G gString
		
readSubject :: IO LSubject
readSubject = do 
	ec	<- promptLine "ECTS (Float): "
	sw	<- promptLine "SWS (Float): "
	ab	<- promptLine "Name (String): "
	t	<- promptType
	return $ createSubject (read ec) (read sw) ab t
	
readResult :: IO LResult
readResult = do
	d <- promptLine "Day (Int): "
	m <- promptLine "Month (Int): "
	y <- promptLine "Year (Integer): "
	p <- promptLine "Prof (String): "
	g <- promptGrade
	return (createResult (read d, read m, read y) g p)
	
list :: Show a => [a] -> IO ()
list x = list' 1 x where
	list' _ [] = return ()
	list' i (x:xs) = (putStrLn $ (show i) ++ ".) " ++ (show x)) >> list' (succ i) xs
	
readIndex :: IO Int
readIndex = do
	ind <- promptLine "Enter Index: "
	return $ (read ind) -1

pickFromList :: [LSubject] -> IO (Maybe LSubject)
pickFromList x = do
	list x
	ind <- readIndex 
	return $ listToMaybe $ x ^.. ix ind
	
modifyList :: [LSubject] -> IO [LSubject]
modifyList x = do
	s <- promptLine "add | res | rem: "
	case map toLower s of
		"add"	-> readSubject	>>= (\n -> return (x++[n]))
		"res"	-> do
					list x
					ind <- readIndex
					res <- readResult
					return $ x & ix ind . result .~ Just res
		"rem"	-> do
					s2 <- promptLine "sub | res: "
					list x
					ind <- readIndex
					case map toLower s2 of
						"sub"	-> let (ys,zs) = splitAt ind x   in return (ys ++ (tail zs))
						"res"	-> return $ x & ix ind . result .~ Nothing
		""		-> return x
		_		-> modifyList x


runCmd :: [LSubject] -> IO [LSubject]
runCmd x = do
	s <- promptLine "list | mod: "
	case map toLower s of
		"list"	-> list x >> runCmd x
		"mod"	-> modifyList x >>= runCmd
		"" 		-> return x
		_		-> runCmd x
			
run :: [LSubject] -> IO ()
run x = do
	n <- runCmd x
	x <- promptLine "Exit? Y | N: "
	case map toLower x of
		"n" -> run n
		_	-> return ()		
			

runconsole = run [cp,e3,e4,stat]
-- Testdaten
so = addResult (createResult (10,12,2013) (G "+") "Krexner")(createSubject 2 1 "Sophomore" SE) 
e3 = addResult (createResult (10,12,2013) (AT S1) "Pfeiler")(createSubject 6 1 "Einfuehrung in die Physik III" VO) 
e4 = addResult (createResult (19,02,2014) (AT G4) "Pfeiler")(createSubject 6 1 "Einfuehrung in die Physik IV" VO) 
cp = addResult (createResult (26,03,2014) (AT S1) "Neumann")(createSubject 5 1 "Computational Physics" VO)
cp2 = (createSubject 5 1 "Computational Physics II" VO)
stat = addResult (createResult (3,03,2014) (AT N5) "Viertl") (createSubject  3 1   "Statistik und Wahrscheinlichkeitstheorie" VO)
std = [cp,e3,e4,stat,so,cp2]



-- Subject Widget

-- subject :: Behavior (Maybe LSubject) -> UI ((Element,Element),Tidings LSubject)
-- subject bsub = do
	-- entry1 <- UI.entry $ fst . maybe ("","") id <$> bsub
	-- entry2 <- UI.entry $ snd . maybe ("","") id <$> bsub
	
	-- return ((getElement entry1, getElement entry2),(,) <$> UI.userText entry1 <*> UI.userText entry2)
	
	
	
	
-- ÖÖHÖHÖH

subjClasses :: LSubject -> String
subjClasses g = "subject " ++ show (toStatus (maybe def (^.grade) (g ^. result)))

main :: IO ()
main = do
	startGUI defaultConfig { tpStatic = Just "./" } setup

	return ()
	

setup :: Window -> UI ()
setup w = void $ do
    return w UI.# set title "GradesHS"
    UI.addStyleSheet w "concept.css"
    buttons <- mkButtons
    getBody w #+
        [UI.div #. "wrap" #+ (header ++ map UI.element buttons)]

header :: [UI Element]
header =
    [ UI.h1  #+ [string "Grades.HS"]
	,  mkSubjects std
	]

mkSubject :: LSubject -> UI Element
mkSubject subj = do
	brem	<- UI.button#. "sb1"	#+ [string "-"]
	bres	<- UI.button#. "sb2"	#+ [string "R"]
	etitle	<- UI.div	#. "title"	#+
		[	UI.div	#.	"ects"	#+ [string . show $ subj ^. ects]
		,	UI.div #.	"type"	#+ [string . show $ subj ^. ltype]
		,	UI.div #.	"name"	#+ [string $ subj ^. name]
		]
	egrade	<- UI.div	#. "grade" #+ [string . show $ maybe def (^.grade) (subj ^. result)]
	eexam	<- UI.div	#. "exam"	#+
		[	UI.div	#.	"date"	#+ [string $ maybe def (show .(^.date)) (subj ^. result)]
		,	UI.div	#.	"prof"	#+ [string $ maybe def (^.prof) (subj ^. result)]	
		]
	outer <- UI.div		#. (subjClasses subj) UI.# set UI.children [brem,bres,etitle,egrade,eexam]
	return outer
	
mkSubjects :: [LSubject] -> UI Element
mkSubjects s = UI.div #. "subjects"  #+ (map mkSubject s)
	
mkButton :: String -> UI (Element, Element)
mkButton title = do
    button <- UI.button UI.#. "button" UI.#+ [string title]
    view   <- UI.p #+ [UI.element button, string "xD"]
    return (button, view)

	
mkButtons :: UI [Element]
mkButtons = do
    list    <- UI.ul #. "buttons-list"
    
    (button1, view1) <- mkButton button1Title
    
    on UI.hover button1 $ \_ -> do
        UI.element button1 #+ [return button1]
    on UI.leave button1 $ \_ -> do
        UI.element button1 #+ []
    on UI.click button1 $ \_ -> do
        UI.element button1 UI.# set text (button1Title ++ " [pressed]")
        liftIO $ threadDelay $ 1000 *  1
        UI.element list    #+ [UI.li UI.# set html "<b>Delayed</b> result!"]
    
    return [list, view1]

  where button1Title = "Click me, I delay a bit"
        button2Title = "Click me, I work immediately"