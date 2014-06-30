module GradesTypes where
import	Data.Time
import	Data.Char (toUpper)
import	Data.Default

data LGrade = AT LGAustrian | G String deriving (Eq,Ord)
data LGAustrian = S1 | U2 | B3 | G4 | N5 deriving (Eq,Ord)
data LType = VO | VU | UE | PR | SE | T String deriving (Eq)


data LStatus = Todo | Pos | Neg deriving (Eq)					-- Different subject stati

-- | Class that shows that some type can be converted to a LStatus
class  Status a  where
	toStatus :: a -> LStatus

instance Status LGrade where
	toStatus (G "") = Todo
	toStatus (G _)	= Pos
	toStatus (AT g) = toStatus g
	
instance Status LGAustrian where
	toStatus N5 = Neg
	toStatus _ 	= Pos
	
	
-- 1 Subject. Curriculum is made of these. Added grades to do stuff.
data LSubject = Subject
	{ _ects 	:: Double			-- Credits
	, _sws		:: Double			-- Hours/Week
	, _name		:: String			-- "Einführung in die Physik 3"
	, _ltype 	:: LType
	, _result	:: Maybe LResult	-- last try 
	} deriving (Eq)


-- Result of an exam/laboratory/..
data LResult = Result
	{ _date		:: Day		-- Date in the system
	, _grade	:: LGrade	-- Passed with grade
	, _prof		:: String	-- Examinant
	} deriving (Eq)

	
{- internal stuff - should not be exposed to the user -}
-- instance Show LResult where
	-- show r = "   " ++ (show (r^.grade)) ++ ", "  ++ (show (r^.date)) ++ " " ++ (r^.prof)

-- instance Show LSubject where
	-- show s = fillTo 5 (show (s^.ltype)) ++ "| " ++ fillTo 10 (s^.name) ++ "\t(" ++ show (s^.ects) ++ ")"
		-- ++  maybe "" ((":\t" ++) . show) (s^.result) where
-- fillTo :: Int -> String -> String
-- fillTo n s = (take n s) ++ take (n-(length s)) (repeat ' ')
	
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

-- | "cast" from string to wanted type
class Read' a where
	fromString' :: String -> a

instance Read' LType where
	fromString' s = case map toUpper s of
		"VO"-> VO
		"VU"-> VU
		"SE"-> SE
		"UE"-> UE
		"PR"-> PR
		_	-> T s

instance Read' LGrade where
	fromString' s = case map toUpper s of
		"1"-> AT S1
		"2"-> AT U2
		"3"-> AT B3
		"4"-> AT G4
		"5"-> AT N5
		_	->G s

-- | Default values for custom types
instance Default LResult where
	def = Result (fromGregorian 0 0 0) (G "") ""
instance Default LSubject where
	def = Subject 0 0 "" (T "") Nothing
instance Default LGrade where
	def = G ""

	
{- Read' is easier and less error prone -}
-- for read implementation
-- import				Text.ParserCombinators.ReadP (ReadS, readP_to_S)
-- import qualified	GHC.Read as R
-- import qualified	Text.Read.Lex as L
-- -- import			Text.ParserCombinators.ReadPrec
-- -- import qualified	Text.ParserCombinators.ReadP as P
-- instance Read LGrade where
  -- readPrec =
    -- R.parens
    -- ( do L.Ident s <- R.lexP
         -- case s of
			-- "G1"-> return (AT S1)
			-- "G2"-> return (AT U2)
			-- "G3"-> return (AT B3)
			-- "G4"-> return (AT G4)
			-- "G5"-> return (AT N5)
			-- _	-> return (G (tail s))
    -- )
  -- readListPrec = R.readListPrecDefault 
  -- readList     = R.readListDefault