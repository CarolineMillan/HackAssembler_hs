module Main where

import Debug.Trace (trace)
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.Char (isSpace)
import Data.Char (isDigit)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map (insert)
import qualified Data.Map as Map

-------------------------------------------------------------------------------------
----------------------------------- MAIN FUNC ---------------------------------------
-------------------------------------------------------------------------------------

main :: IO ()
main = do
	args <- getArgs
	case args of
		[input_fp, output_fp] -> do
			content <- readFile input_fp
			let binary = assemble content
			writeFile output_fp binary
		_ -> putStrLn "Usage: HackAssembler_hs <input.asm> <output.hack>"

-------------------------------------------------------------------------------------
----------------------------------- ASSEMBLER ---------------------------------------
-------------------------------------------------------------------------------------

--assemble :: String -> String
--assemble = secondPass . firstPass . cleanUp

assemble :: String -> String
assemble x =
    let cleaned = cleanUp x
        symTbl = firstPass cleaned
    in  secondPass symTbl (map snd cleaned)
    --in  secondPass (trace (show symTbl) symTbl) (map snd cleaned) -- debug
firstPass :: [(Integer, String)] -> Map String Integer
firstPass s = snd $ foldl extractLabels (0, symbolTable) s

secondPass :: Map String Integer -> [String] -> String
secondPass symTbl s = 
    let (_, _, outLines) = foldl translateLine (symTbl, 16, []) s
    in  intercalate "\n" (removeEmptyLines outLines)
-- intercalate puts the [string] together into a string with a "\n" between each one. It does not append a "\n" to the end of the file (unlike unlines)

-------------------------------------------------------------------------------------
----------------------------------- CLEAN UP ----------------------------------------
-------------------------------------------------------------------------------------

-- remove comments and whitespace (inc inline) and empty lines
cleanUp :: String -> [(Integer, String)]
cleanUp s = zip [0..] (map (removeWhitespace . removeComment) (lines s))

-- use a filter to remove all whitespace characters
removeWhitespace :: String -> String
removeWhitespace s = filter (not . isSpace) s

-- remove all comments (even inline) from a single line
-- it either begins with a '//', has an inline '//' or doesn't contain a comment
removeComment :: String -> String
removeComment s
	| take 2 s == "//" = ""
	| otherwise = head (splitOn "//" s)

-- remove empty lines
removeEmptyLines :: [String] -> [String]
removeEmptyLines s = filter (not . null) s
-------------------------------------------------------------------------------------
----------------------------------- FIRST PASS --------------------------------------
-------------------------------------------------------------------------------------

-- define the initial symbolTable as a Map
symbolTable :: Map String Integer
symbolTable = Map.fromList initialSymbols
	where
		initialSymbols = [("R0", 0),("R1", 1),("R2", 2),("R3", 3),("R4", 4),("R5", 5),("R6", 6),("R7", 7),("R8", 8),("R9", 9),("R10", 10),("R11", 11),("R12", 12),("R13", 13),("R14", 14),("R15", 15),("SP", 0),("LCL", 1),("ARG", 2),("THIS", 3),("THAT", 4),("SCREEN", 16384),("KBD", 24576)]

-- loop through the input string, and add any symbols to the symbolTable (or technically create a new symbolTable)
-- use insert and lookup on symbolTable to set and access elements
-- pc is the instruction counter, the integer coupled with the string is the raw line number
extractLabels :: (Integer, Map String Integer) -> (Integer, String) -> (Integer, Map String Integer)
extractLabels (pc, st) (_, "") = (pc, st)
extractLabels (pc, st) (_, line@(x:_))
	| x == '(' = (pc, (insert key pc st)) -- add key = xs minus ')' to symbol table and return new symbol table and an empty string
	| otherwise = (pc+1, st)
	where
		key = fromMaybe "" $ safeInit (tail line)

-- safeInit is just Data.List.init wrapped to avoid crashing on empty lists
safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (init xs)
-- TODO: look up the keywords Nothing and Just

-------------------------------------------------------------------------------------
----------------------------------- SECOND PASS -------------------------------------
-------------------------------------------------------------------------------------

-- this takes the previous state and returns the current state of the symbol table, so that it can add variables to it
translateLine :: (Map String Integer, Integer, [String]) -> String -> (Map String Integer, Integer, [String])
translateLine (symTbl, i, outLines) "" = (symTbl, i, outLines)
translateLine (symTbl, i, outLines) (x:xs)
--	| x == '@' = ('0' : binaryRep xs) -- A intstruction (this will change once you have to check for labels)
    | x == '@' = aInstruction (symTbl, i, outLines) xs
    | x == '(' = (symTbl, i, outLines) -- label
	| otherwise = (symTbl, i, outLines ++ [cInstruction (x:xs)]) -- C instruction

-------------------------------------------------------------------------------------
----------------------------------- A INSTRUCTION -----------------------------------
-------------------------------------------------------------------------------------

aInstruction :: (Map String Integer, Integer, [String]) -> String -> (Map String Integer, Integer, [String])
aInstruction (symTbl, i, outLines) s -- = -- (symTbl, i, outLines ++ [binaryRep s])
    | all isDigit s = (symTbl, i, outLines ++ ['0' : binaryRep s]) -- no variable
    | otherwise     = case Map.lookup s symTbl of
                        Just address -> (symTbl, i, outLines ++ ['0' : binaryRep (show address)]) -- 
                        Nothing -> (Map.insert s i symTbl, i+1, outLines ++ ['0' : binaryRep (show i)]) -- add it to the symbol table

    -- (symTbl, i, outLines) -- placeholder, handle symbols later

binaryRep :: String -> String
binaryRep s = padded
	where
		padded = replicate (15-(length shortBinary)) '0' ++ shortBinary -- adds the remaining 0s to the front of the binary representation
		shortBinary = intToBinary x
		x = stringToInt s

intToBinary :: Int -> String
intToBinary n = showIntAtBase 2 intToDigit n ""

stringToInt :: String -> Int
stringToInt s = read s

-------------------------------------------------------------------------------------
----------------------------------- C INSTRUCTION -----------------------------------
-------------------------------------------------------------------------------------

cInstruction :: String -> String
cInstruction s = "111" ++ assembleComp comp ++  assembleDest dest ++ assembleJump jump
	where
		(dest, comp) = parseDest minusJump
		(minusJump, jump) = parseJump s


parseDest :: String -> (String, String)
parseDest s =
	case (break (== '=') s) of
		(d,_:x) -> (d,x) -- wildcard to remove seperator = infront of second part
		(x, _) -> ("",x)

parseJump :: String -> (String, String)
parseJump s =
	case (break (== ';') s) of
		(x,_:j) -> (x,j) -- wildcard to get rid of separator ; in from of j
		(x, _) -> (x,"")

assembleComp :: String -> String
assembleComp s
	| s == "0" = "0101010"
	| s == "1" = "0111111"
	| s == "-1" = "0111010"
	| s == "D" = "0001100"
	| s == "A" = "0110000"
	| s == "M" = "1110000"
	| s == "!D" = "0001101"
	| s == "!A" = "0110001"
	| s == "!M" = "1110001"
	| s == "-D" = "0001111"
	| s == "-A" = "0110011"
	| s == "-M" = "1110011"
	| s == "D+1" = "0011111"
	| s == "A+1" = "0110111"
	| s == "M+1" = "1110111"
	| s == "D-1" = "0001110"
	| s == "A-1" = "0110010"
	| s == "M-1" = "1110010"
	| s == "D+A" = "0000010"
	| s == "D+M" = "1000010"
	| s == "D-A" = "0010011"
	| s == "D-M" = "1010011"
	| s == "A-D" = "0000111"
	| s == "M-D" = "1000111"
	| s == "D&A" = "0000000"
	| s == "D&M" = "1000000"
	| s == "D|A" = "0010101"
	| s == "D|M" = "1010101"
	| otherwise = error ("Invalid Comp: " ++ s)

assembleDest :: String -> String
assembleDest s
	| s == "" = "000"
	| s == "M" = "001"
	| s == "D" = "010"
	| s == "DM" = "011"
	| s == "MD" = "011"
	| s == "A" = "100"
	| s == "AM" = "101"
	| s == "AD" = "110"
	| s == "ADM" = "111"
	| otherwise = error ("Invalid Dest: " ++ s)

assembleJump :: String -> String
assembleJump s
	| s == "" = "000"
	| s == "JGT" = "001"
	| s == "JEQ" = "010"
	| s == "JGE" = "011"
	| s == "JLT" = "100"
	| s == "JNE" = "101"
	| s == "JLE" = "110"
	| s == "JMP" = "111"
	| otherwise = error ("Invalid Jump: " ++ s ++ ".")







-- dest = comp; jump

--parseComp :: String -> String
--parseComp s =
--	case (split (== ';') (split (== '=') s)) of
--		[d,c,j] -> c
--		-- what other cases?
--data CInstruction = Dest String Cond String Jump String

-- THIS WORKS SO FAR
--
-- NOW I'VE JSUT GOT TO ACTUALLY WRITE AN ASSEMBLER
--
-- USE lines x to seperate the string into lines (splits on \n)
-- the parse each line
-- this is instructions, do it the haskell way (maps)
--
-- so second pass is mapping a string to the binary string
-- do this using a map (this map will take a symbolTable and a string as input) on each line
--
-- HOW DO I CREATE THE SYMBOL TABLE?
-- DO I STILL NEED TO DO TWO PASSES?

-- so i want a symbol table
-- main will open the file, read it, send it to the assembler, then output the result to another file
