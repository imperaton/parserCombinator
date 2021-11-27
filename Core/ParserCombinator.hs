module Core.ParserCombinator where

import Control.Applicative as Applic
import Data.Bifunctor
import Text.Regex.TDFA as Regex
import Text.Regex.TDFA.Text ()
import Data.Functor as Functor


-- ===================================================================================================
-- | Define a Custom Error Type
-- ===================================================================================================
data Error = Error String
instance Show Error where
    show (Error s) = "'" ++ s ++ "'"


-- ===================================================================================================
-- | Define ParserCombinator Monad by implementing `return` and `(>>=)` and proof the monad laws.
-- |
-- | The functions `fmap`, `pure` and `liftA2` are implemente using `return` and `(>>=)`.
-- ===================================================================================================
data Parser token output = ParserCons ([token] -> (Either Error ([token], output)))

runParser :: Parser token output -> [token] -> Either Error ([token], output)
runParser (ParserCons parser) ts = parser ts

runParserAndPrint :: Parser token String -> [token] -> String
runParserAndPrint parser ts = either
    (\err -> "Error:\n" ++ show err)
    (\pair -> output pair)
    (runParser parser ts)

leftover = fst; output = snd

instance Functor (Parser token) where
    fmap f parser = parser >>= (pure . f)
    -- Alternative: fmap f (ParserCons parser) = ParserCons $ (fmap . fmap . fmap) f parser
instance Applicative (Parser token) where
    pure = return
    liftA2 f ma mb = ma >>= (\a -> return (f a)) >>= (\g-> mb >>= (return . g))
instance Monad (Parser token) where
    return output = ParserCons $ \tokens -> Right (tokens, output)
    (ParserCons parser) >>= f = ParserCons $ \tokens ->
         do pair <- parser tokens
            let interimParser = (f (output pair))
                interimTokens = (leftover pair)
            runParser interimParser interimTokens
    -- =============================Without do notation=================================
    -- | (ParserCons parser) >>= f = ParserCons $ \tokens ->
    -- |          parser tokens >>= \ pair ->
    -- |              let interimParser = (f (output pair))
    -- |                  interimtokens = (leftover pair)
    -- |              in runParser interimParser interimtokens
    -- =================================================================================

-- ===================================================================================================
-- | Proof Monad Laws:
-- | (I)    (return x) >>= f ==== f x
-- | (II)   m >>= return ==== m
-- | (III)  (m >>= f) >>= g ==== m >>= (\x -> f x >>= g)
-- ===================================================================================================
-- | Where (m = Parser token) and
-- | (A) return :: output -> Parser token output
-- |     return output = ParserCons $ \tokens -> Right (tokens, output)
-- | (B) (>>=) :: Parser token output -> (output -> Parser token output') -> Parser token output'
-- |     (ParserCons parser) >>= f = ParserCons $ \tokens ->
-- |         parser tokens >>= \pair -> runParser (f (output pair)) (leftover pair)
-- ===================================================================================================
-- | (I)   (return out) >>= f
-- |       =    ParserCons (\ts -> Right (ts, out)) >>= f
-- |       =    ParserCons $ \tokens ->
-- |                (\ts -> Right (ts, out)) tokens >>=
-- |                    \pair -> runParser (f (output pair)) (leftover pair)
-- |       =    ParserCons $ \tokens ->
-- |                Right (tokens, out)) >>=
-- |                    \pair -> runParser (f (output pair)) (leftover pair)
-- |       =    ParserCons $ \tokens -> runParser (f out) tokens
-- |       =    ParserCons $ runParser (f out)
-- |       =    f out
-- ===================================================================================================
-- | (II)  (ParserCons parser) >>= return
-- |       =    ParserCons $ \tokens ->
-- |                parser tokens >>= \ pair -> runParser (return (output pair)) (leftover pair)
-- |       =    ParserCons $ \tokens ->
-- |                parser tokens >>= \ pair ->
-- |                    runParser (ParserCons $ \ts -> Right (ts, (output pair))) (leftover pair)
-- |       =    ParserCons $ \tokens ->
-- |                parser tokens >>= \ pair -> Right (leftover pair, output pair)
-- |       =    ParserCons $ \tokens -> parser tokens >>= \ pair -> Right pair
-- |       =    ParserCons $ \tokens -> parser tokens >>= return
-- |       =    ParserCons $ \tokens -> parser tokens
-- |       =    ParserCons parser
-- ===================================================================================================
-- | (III) ((ParserCons parser) >>= f) >>= g
-- |       =    ParserCons (\tokens ->
-- |                parser tokens >>= \ pair ->
-- |                    runParser (f (output pair)) (leftover pair)) >>= g
-- |       =    ParserCons $ \tokens ->
-- |                (\tokens' -> parser tokens' >>=
-- |                    \pair -> runParser (f (output pair)) (leftover pair)) tokens
-- |                >>= \pair -> runParser (g (output pair)) (leftover pair)
-- |       =    ParserCons $ \tokens ->
-- |                parser tokens
-- |                    >>= \pair -> runParser (f (output pair)) (leftover pair)
-- |                    >>= \pair -> runParser (g (output pair)) (leftover pair)
-- |
-- |       (ParserCons parser) >>= (\out -> f out >>= g)
-- |       =    ParserCons $ \tokens ->
-- |                parser tokens >>= \pair ->
-- |                    runParser ((\out -> f out >>= g) (output pair)) (leftover pair)
-- |       =    ParserCons $ \tokens ->
-- |                parser tokens >>= \pair ->
-- |                    runParser (f (output pair) >>= g) (leftover pair)
-- |       =    ParserCons $ \tokens ->
-- |                parser tokens >>= \pair ->
-- |                    runParser
-- |                        (ParserCons (runParser (f (output pair))) >>= g)
-- |                        (leftover pair)
-- |       =    ParserCons $ \tokens ->
-- |                parser tokens >>= \pair ->
-- |                    runParser
-- |                        (ParserCons $ \tokens' ->
-- |                            (runParser (f (output pair))) tokens'
-- |                                >>= \pair' -> runParser (g (output pair')) (leftover pair'))
-- |                        (leftover pair)
-- |       =    ParserCons $ \tokens ->
-- |                parser tokens
-- |                    >>= \pair -> runParser (f (output pair))) (leftover pair)
-- |                    >>= \pair'-> runParser (g (output pair')) (leftover pair')
-- ===================================================================================================




-- ===================================================================================================
-- Assign name to the `return` method of the monad `Parser token`
-- ===================================================================================================
returnParser :: output -> Parser token output
returnParser = return


-- ===================================================================================================
-- | Implement Alternative instance for (Parser token). Then the operator (<|>) can be used
-- | to construct a parser which constitued the alternative of two parsers.
-- |
-- | Example:
-- |   runParser (parser <|> parser') "foo"
-- ===================================================================================================
instance Alternative (Parser token) where
    empty = ParserCons $ \tokens -> Left $ Error "An empty parser was applied"
    (ParserCons parser) <|> (ParserCons parser') = ParserCons $ parser''
        where
            parser'' tokens =
                either
                (\a -> either
                            (\a' -> Left (Error "None of given alternative parsers were successful."))
                            (\b -> Right b)
                            (parser' tokens))
                (\b -> Right b)
                (parser tokens)


-- ===================================================================================================
-- | Modify a parser such that it returns `Nothing` while simply returning the input tokens when the
-- | underlying fails and `Just x` if the underlying parser returns `x`.
-- ===================================================================================================
tryParser :: Parser token output -> Parser token (Either Error output)
tryParser (ParserCons parser) = ParserCons $ \tokens ->
            either
                (\err -> Right (tokens, Left err))
                (\(ts, out) -> Right (ts, Right out))
                (parser tokens)


-- ===================================================================================================
-- Repeatedly run a parser which depends on one variable and whose output is of the same type
-- as that variable. The parser is applied until the next application fails.
-- ===================================================================================================
recurseParser :: (b -> Parser a b) -> (b -> Parser a b)
recurseParser base = \x -> (tryParser (base x) >>= either (\_ -> return x) (recurseParser base))


-- ===================================================================================================
-- | Create a single parser from a list of parser storing the result of all parsers in a list.
-- ===================================================================================================
parserList :: [Parser token output] -> Parser token [output]
parserList = sequence


-- ===================================================================================================
-- | Match a single token from the input token list.
-- ===================================================================================================
matchSingle :: (Eq token, Show token) => token -> Parser token token
matchSingle t = ParserCons $ parser where
        parser [] = Left (Error "Match failed due to empty match list.")
        parser (t':t's) = if t==t'
            then Right (t's, t)
            else Left $ Error $ "Match failed. Expected: " ++ (show t) ++
                                ", Actual: " ++ (show t')


-- ===================================================================================================
-- | Match any of multiple tokens.
-- ===================================================================================================
matchAnyOf :: (Eq token, Show token) => [token] -> Parser token token
matchAnyOf tokens = foldr (<|>) Applic.empty (Prelude.map matchSingle tokens)


-- ===================================================================================================
-- | Match space or tab.
-- ===================================================================================================
matchWhitespace :: Parser Char Char
matchWhitespace = matchAnyOf " \t"


-- ===================================================================================================
-- | Match EOL. The parser also matches an arbitrary amount of whitespace before the newline
-- | character '\n'.
-- ===================================================================================================
matchEOL :: Parser Char ()
matchEOL = (do
        many matchWhitespace
        matchSingle '\n') <&> (const ())


-- ===================================================================================================
-- | Match EOF. Check if the input token list is empty
-- ===================================================================================================
checkEOF :: Parser Char ()
checkEOF = ParserCons $ parser where
                   parser [] = Right ([], ())
                   parser tokens = Left (Error "Check for EOF failed")


-- ===================================================================================================
-- | Create a string parser which matches a regex expression to the beginning of the input string.
-- |
-- | The output of the resulting parser (if successful) is a pair `(s :: String, ss :: [String])`
-- | where the `s` contains the matched string and `ss` is a list of the matched subgroups.
-- |
-- | Example:
-- |   runParser (matchRegex "([0-9]*)-([a-z]*)") "0123-abcd"
-- |       = Right ("", ("0123-abcd", ["0123","abcd"]))
-- |
-- | Note:
-- |   The regex package `Text.Regex.TDFA` used here employs the character ` as beginning of
-- |   the string. The Parser `matchRegex <string>` only succeeds if the pattern can be found
-- |   at the beginning of the input string.
-- ===================================================================================================
matchRegex :: String -> Parser Char (String, [String])
matchRegex regex = ParserCons $ \string ->
        let result = string =~ ("\\`" ++ regex) :: (String, String, String, [String])
            matchString = (\(_,s,_,_) -> s) result
            submatchesStringList = (\(_,_,_,s) -> s) result
            remainingString = (\(_,_,s,_) -> s) result
        in if null matchString
              then Left $ Error $ "Regex match '" ++ regex ++ "' failed. Current tokens: " ++ string
              else Right (remainingString, (matchString, submatchesStringList))
regexMainMatch :: String -> Parser Char String
regexMainMatch regex = matchRegex regex <&> fst
regexSubMatches :: String -> Parser Char [String]
regexSubMatches regex = matchRegex regex <&> snd
