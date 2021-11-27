 -- Haskell Libraries
import System.Environment
import System.Exit
import Data.Map.Strict as Map
import Control.Applicative as Applic
import Data.Functor as Functor

-- Custom Parser Library
import Core.ParserCombinator

-- Import type `Configs`
import TranslateConfigFiles.ConfigFilesCommon



-- ===================================================================================================
-- | Read config file of pattern
-- |
-- | ```
-- | Section1.setting1 = value1
-- | Section1.setting2=value2
-- |
-- | Section2.setting1=value1
-- | ```
-- |
-- | to data structure of type `Configs = Map String [(String, String)]`
-- ===================================================================================================
readSetting :: Configs -> Parser Char Configs
readSetting configs = do
    section <- regexMainMatch "[a-zA-Z][a-zA-Z0-9]*"
    matchSingle '.'
    key <- regexMainMatch "[a-zA-Z][a-zA-Z0-9]*"
    many matchWhitespace; matchSingle '='; many matchWhitespace
    value <- regexMainMatch "[a-zA-Z0-9]*"
    some matchEOL
    returnParser (insertConfig section (key, value) configs)
readSettings :: Configs -> Parser Char Configs
readSettings configs = do
    many matchEOL
    configs' <- recurseParser readSetting configs
    many matchEOL
    checkEOF
    returnParser configs'


-- ===================================================================================================
-- | Convert a map of type `Configs = Map String [(String, String)]` to string like
-- |
-- | ```
-- | [Section1]
-- | setting1 = value1
-- | setting2=value2
-- | [Section2]
-- | setting1=value1
-- | ```
-- ===================================================================================================
writeSections :: Configs -> String
writeSections configs = foldlWithKey f "" configs
        where
            f out k v = out ++ "[" ++ k ++ "]\n" ++
                unlines [setting ++ "=" ++ value | (setting, value ) <- v] ++ "\n"


-- ===================================================================================================
-- | Construct a translation function
-- ===================================================================================================
simpleToSections :: String -> String
simpleToSections = runParserAndPrint $ readSettings (Map.empty :: Configs)
                                        <&> writeSections


-- ===================================================================================================
-- | Add a main method which parses two inputs from the command line (input_file_path and
-- | output_file_path) and translate the contents of the first file and stores the result
-- | in the second file.
-- |
-- | Sources:
-- |   - https://wiki.haskell.org/Tutorials/Programming_Haskell/Argument_handling
-- ===================================================================================================
main = do
    args <- getArgs
    [inputString, outputFile] <- parse args
    writeFile outputFile (simpleToSections inputString)

parse :: [String] -> IO [String]
parse [inputFile, outputFile] = do
    inputString <- readFile inputFile
    return [inputString, outputFile]
parse _ = usage >> exit

usage   = putStrLn "Usage: SectionsToSimple [input_file_path] [output_file_path]"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
