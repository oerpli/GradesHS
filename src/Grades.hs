{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}
module GradesHS where

import Control.Applicative ((<$>), (<*>))
import Control.Lens
import Control.Monad.State (State, execState, get)
import Control.Monad (when)
import Data.Maybe
import Data.Aeson
import Data.Time
import Data.Text
import Data.Set (Set, member, empty, insert, delete)

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import System.Random (randomRs, newStdGen)

-- Pure data type for representing the game state

data LGrade = S1 | U2 | B3 | G4 | N5 | G String deriving (Show)
data LType = VO | VU | UE | PR | SE | L String deriving (Show)

data LSubject = Subject
	{ _ects 	:: Float			-- Credits
	, _sws		:: Float			-- Hours/Week
	, _abbr		:: String					-- e.g. "E3" for "Einführung in die Physik 3"
	-- , _name		:: Maybe String				-- "Einführung in die Physik 3"
	, _ltype 	:: Maybe LType
	, _result	:: Maybe LResult	-- last try 
	} deriving (Show)

data LResult = Result
	{ _date		:: Day		-- Date in the system
	, _grade	:: LGrade	-- Passed with grade
	, _prof		:: String		-- Examinant
	} deriving (Show)
	

makeLenses ''LResult
makeLenses ''LSubject

sprint :: LSubject -> IO()
sprint s = do
	putStrLn $ (show (s^.ects)) ++ "ECTS - " ++ s^.abbr ++ " - "
	maybe (return ()) rprint (s^.result)

rprint :: LResult -> IO()
rprint r = do
	putStrLn $ "  " ++ (show (r^.grade)) ++ " - "  ++(show (r^.date))

initLSubject = Subject
	{ _ects = 0
	, _sws	= 0
	, _abbr	= ""
	-- , _name	= Nothing
	, _ltype = Nothing
	, _result = Nothing
	}
initLResult = Result
	{ _date = fromGregorian 0 0 0
	, _grade = G ""
	, _prof = ""
	}

createSubject (e,s) (a,t)  = 
	(ects .~ e) . (sws .~ s) . (abbr .~ a) . (ltype .~ Just t)  $ initLSubject

addResult  (d,m,y) g p =
	result .~ Just ((date .~ fromGregorian y m d) . (grade .~ g) . (prof .~ p) $ initLResult)

e3 = addResult (19,2,2014) G4 "Pfeiler" $ createSubject (1,1) ("E3",VO) 

main :: IO()
main = sprint e3
