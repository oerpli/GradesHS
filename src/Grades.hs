{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}
module GradesHS where

-- import Control.Applicative ((<$>), (<*>))
import Control.Lens
import Control.Monad.State (State, execState, get)
import Control.Monad (when)
import Data.Maybe
import Data.Aeson
import Data.Time
import Data.Default
-- import Data.Text
import Data.Map
import Data.Set (Set, member, empty, insert, delete)

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import System.Random (randomRs, newStdGen)

-- Pure data type for representing the game state

data LGrade = S1 | U2 | B3 | G4 | N5 | G String deriving (Show)
data LType = VO | VU | UE | PR | SE | L String

-- Stores the subjects added so far.
data LState = State 
	{ _subjects	:: [LSubject]
	} 

-- 1 Subject. Curriculum is made of this thing. Added grades to do stuff.
data LSubject = Subject
	{ _ects 	:: Float			-- Credits
	, _sws		:: Float			-- Hours/Week
	, _abbr		:: String			-- e.g. "E3" for "Einführung in die Physik 3"
-- 	, _name		:: Maybe String		-- "Einführung in die Physik 3"
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


instance Show LType where
	show (L s) = s
	show SE = "SE"
	show UE = "UE"
	show VO = "VO"
	show VU = "VU"
	show PR = "PR"

instance Show LSubject where
	show s = show (s^.ltype) ++ "|" ++ fillTo 10 (s^.abbr) ++ "\t(" ++ show (s^.ects) ++ ")"
		++  maybe "" ((":\t" ++) . show) (s^.result)
instance Show LResult where
	show r = "   " ++ (show (r^.grade)) ++ ", "  ++ (show (r^.date)) ++ " " ++ (r^.prof)

fillTo :: Int -> String -> String
fillTo n s = (take n s) ++ take (n-(length s)) (repeat ' ')


instance Default LState where
	def = State []
instance Default LResult where
	def = Result (fromGregorian 0 0 0) (G "") ""
instance Default LSubject where
	def = Subject 0 0 "" (L "") Nothing
	
	
	
createSubject :: (Float, Float) -> (String, LType) -> LSubject
createSubject (e,s) (a,t)  = def 
	& ects .~ e
	& sws .~ s
	& abbr .~ a
	& ltype .~ t

addResult :: (Int, Int, Integer) -> LGrade -> String -> LSubject -> LSubject
addResult  d g p = result .~ Just (cResult d g p) where 
	cResult (d,m,y) g p = def 
		& date .~ fromGregorian y m d
		& grade .~ g
		& prof .~ p


e3 = addResult (10,12,2013) S1 "Pfeiler" $ createSubject (6,1) ("E3",VO) 
e4 = addResult (19,02,2014) G4 "Pfeiler" $ createSubject (6,1) ("E4",VO) 
cp = addResult (26,03,2014) S1 "Neumann" $ createSubject (5,1) ("Comp. Physics",VO)
stat = createSubject (3,1) ("Statistik",VO)

-- User input etc.
promptLine :: String -> IO String
promptLine prompt = do
    putStr $ prompt
    getLine

promptType :: IO LType
promptType = do
	tString <- promptLine "Type (VO|VU|SE|UE|PR|L [..]): "
	case tString of
		"VO"-> return VO
		"VU"-> return VU
		"SE"-> return SE
		"UE"-> return UE
		"PR"-> return PR
		"L"	-> do
			str <- promptLine "Typ eingeben: "
			return $L str
		_ -> promptType


--match string - if it is a known command execute and return modified state.
modifyState :: LState -> String -> LState
modifyState s cmd = s --todo

readSubject :: IO LSubject
readSubject = do 
	ec	<- promptLine "ECTS (float): "
	sw	<- promptLine "SWS (float): "
	ab	<- promptLine "Name (string): "
	t	<- promptType
	return (createSubject (read ec,read sw) (ab,t))

main :: IO()
main = do
	putStrLn $ show e3
	putStrLn $ show e4
	putStrLn $ show stat
	putStrLn $ show cp
