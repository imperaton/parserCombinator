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
-- | [Section1]
-- | setting1 = value1
-- | setting2=value2
-- | [Section2]
-- | setting1=value1
-- | ```
-- |
-- | to data structure of type `Configs = Map String [(String, String)]`
-- ===================================================================================================
-- Read one line `setting=value\n` and insert `(setting, value)` into the map.
readSetting :: String -> Configs -> Parser Char Configs
readSetting section configs = do
        key <- regexMainMatch "[a-zA-Z][a-zA-Z0-9]*"
        many matchWhitespace; matchSingle '='; many matchWhitespace
        value <- regexMainMatch "[a-zA-Z0-9]*"
        some matchEOL
        returnParser (insertConfig section (key, value) configs)
-- Read one line `[Section1]\n` and read an arbitrary amount of settings by using `recurseParser`
-- and `readSettings`.
readSection :: Configs -> Parser Char Configs
readSection configs = do
        section <- regexSubMatches "\\[([a-zA-Z][a-zA-Z0-9]*)\\]" <&> head
        some matchEOL
        recurseParser (readSetting section) configs
-- Read any number of Sections and check EOF.
readSections :: Configs -> Parser Char Configs
readSections configs = do
        many matchEOL
        configs' <- recurseParser readSection configs
        many matchEOL
        checkEOF
        returnParser configs'


-- ===================================================================================================
-- | Convert a map of type `Configs = Map String [(String, String)]` to string like
-- |
-- | ```
-- | Section1.setting1=value1
-- | Section1.setting2=value2
-- |
-- | Section2.setting1=value1
-- | ```
-- ===================================================================================================
writeSimple :: Configs -> String
writeSimple configs = foldlWithKey f "" configs
        where
            f out k v = out ++
                unlines [k ++ "." ++ setting ++ "=" ++ value | (setting, value ) <- v] ++ "\n"


-- ===================================================================================================
-- | Construct a translation function
-- ===================================================================================================
sectionsToSimple :: String -> String
sectionsToSimple = runParserAndPrint $ readSections (Map.empty :: Configs)
                                        <&> writeSimple


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
    writeFile outputFile (sectionsToSimple inputString)

parse :: [String] -> IO [String]
parse [inputFile, outputFile] = do
    inputString <- readFile inputFile
    return [inputString, outputFile]
parse _ = usage >> exit

usage   = putStrLn "Usage: SectionsToSimple [input_file_path] [output_file_path]"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)

