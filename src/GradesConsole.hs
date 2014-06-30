{-User input etc.-}
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
	g <- promptLine "Grade (1,2,3,4,5, [..]): "
	return (createResult' (d, m, y) g p)
	
list :: Show a => [a] -> IO ()
list x = list' 1 x where
	list' _ [] = return ()
	list' i (x:xs) = (putStrLn $ (show i) ++ ".) " ++ (show x)) >> list' (succ i) xs
	
readIndex :: IO Int
readIndex = do
	ind <- promptLine "Enter Index: "
	return $ (read ind) -1

modifyList :: [LSubject] -> IO [LSubject]
modifyList x = do
	s <- promptLine "add | res | rem: "
	case map toLower s of
		"add"	-> do 
					sub <- readSubject
					return $ applyAction (AddSub sub) x
		"res"	-> do
					list x
					ind <- readIndex
					res <- readResult
					return $ applyAction (AddRes ind res) x
		"rem"	-> do
					s2 <- promptLine "sub | res: "
					list x
					ind <- readIndex
					case map toLower s2 of
						"sub"	-> return $ applyAction (RemSub ind) x
						"res"	-> return $ applyAction (RemRes ind) x
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

runconsole = run std