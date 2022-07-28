{-----------------------------------------------------------------------------------------------
This program can be used to generate a Haskell program that recognizes a given deterministic
context-free language specified by a DPDA source file of the following form:

  The first line contains a single integer representing the name of the initial state.
  The second line contains a single integer representing the name of the final state.
  Remaining lines are rules the form "a,b,c->d,e" which can be read as:
    At state a, if the head of the input is b and the top of the stack is c, then pop c,
    push d, and go to state e.

Example: A source file for the language of properly nested parenthesis will look like:
  0
  3
  0, _, _ -> $, 1
  1, (, _ -> (, 1
  1, ), ( -> _, 2
  2, (, _ -> (, 1
  2, ), ( -> _, 2
  2, _, $ -> _, 3

Important Notes:
  - The machine should be designed with exactly one accepting state.
  - The '$' character should be used as the stack symbol.
  - The '_' character should be used as the empty-string symbol (epsilon).
  - This program is not limited to single-character symbols. For example, in the example given
    above, you could replace ever instance of ( with ([ and every instance of ) with ]) as the
    source for a DPDA that treats ([ and ]) as parenthetical pairs, rather than ( and ). i.e.
    rather than recognizing (), (()), (()()), etc, the language will recognize ([]), ([([])]),
    ([([])([])]), etc.
-----------------------------------------------------------------------------------------------}

import System.IO
import Prelude

import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (takeDirectory)

------------------------------------------------------------------------------------------------------------------------------------------
-- Configure I/O file locations here.

pda_src_file = "./user-files/source-lines.txt"
pda_dest_file = "./user-files/result.hs"

------------------------------------------------------------------------------------------------------------------------------------------
-- Special symbols.
epsilon_sym = "[]"

------------------------------------------------------------------------------------------------------------------------------------------
-- General utilities.

-- Access functions for individual components of a triple (3-tuple).
fst_3 (a, _, _) = a
snd_3 (_, b, _) = b
trd_3 (_, _, c) = c

-- Creates a new file with given content at a given location.
createAndWriteFile :: FilePath -> String -> IO ()
createAndWriteFile path content = do
  createDirectoryIfMissing True $ takeDirectory path
  writeFile path content

------------------------------------------------------------------------------------------------------------------------------------------
-- Skeleton code to be added to all generated DPDA programs.

header_comment = "-- This code was automatically generated. \n\n"

pda_result_type = [ "-- This class represents the results of the recognizer."
                  , "data PDA_Result = Accept | Reject"
                  , "instance Show PDA_Result"
                  , "  where"
                  , "    show Accept = \"Accept\""
                  , "    show Reject = \"Reject\""]

output_base_fst startstate = [ "-- This is the scanner that you should use if your JFLAP DPDA was written in multi"
                             , "-- character mode. To use it, apply this function to a list of strings, where each string"
                             , "-- in the list represents a symbol in the language. For example, if your JFLAP DPDA is"
                             , "-- written to recognize the language (ab)^k(cd)^k, then you can recognize \"abcd\" by"
                             , "-- writing scan [\"ab\", \"cd\"], or \"ababcdcd\" by writing [\"ab\", \"ab\", \"cd\", \"cd\"], etc."
                             , "scan :: [String] -> PDA_Result"
                             , "scan input = scan' input [] \"" ++ startstate ++ "\""
                             , "  where"]

output_base_lst :: [Char] -> [[Char]]
output_base_lst finalstate = [ "    scan' [] [] \"" ++ finalstate ++ "\" = Accept"
                             , "    scan' _ _ _ = Reject"]

output_additional_functions :: [[Char]]
output_additional_functions = [ "\n\n-- This is the scanner that you should use if your JFLAP DPDA was written in single"
                              , "-- character mode. To use it, simply apply this function to the strings that you want"
                              , "-- to recognize. For example, if your JFLAP DPDA is written to recognize the language"
                              , "-- a^kb^k, and you want to recognize \"ab\", then simply write scan_char_sequence \"ab\"."
                              , "scan_char_sequence :: String -> PDA_Result", "scan_char_sequence str = scan $ break_string str"
                              , "  where break_string = map (\\ch -> [ch])\n"]

------------------------------------------------------------------------------------------------------------------------------------------

-- Given a string, str, and a character, ch, splitOnCh returns a pair (x,y) of strings
-- where x contains every character in str up to (and excluding) the first occurrence
-- of ch, and y is the remaining characters in str after ch (also excluding ch).
-- If ch is not contained in str, then (x,y) = (str, '').
-- To be clear, any occurrences of ch after the first are ignored.
splitOnCh str ch = splitOnCh' str 0
  where splitOnCh' "" index = (str, "")
        splitOnCh' (ch':str') index =
          if ch' == ch
          then (take index str, drop (index+1) str)
          else splitOnCh' str' (index+1)


-- Given a string, str, and another string, substr, splitOnSubstr returns a pair (x,y) of
-- strings where x contains every character in str up to (and excluding) the first occurrence
-- of substr, and y is the remaining characters in str after substr (also excluding substr).
-- If substr is not contained in str, then (x,y) = (str, '').
-- To be clear, any occurrences of substr after the first are ignored.
splitOnSubstr str substr = splitOnSubstr' str substr (0, 0)
  where splitOnSubstr' "" _ _ = (str, "")
        splitOnSubstr' (ch':str') (subch:[]) (i, j) =
          if ch' == subch
          then (take i str, drop (j+1) str)
          else splitOnSubstr' (drop (i+1) str) substr (i+1, i+1) -- else splitOnSubstr' (drop i str) substr (i, i)
        splitOnSubstr' (ch':str') (subch:substr') (i, j) =
          if ch' == subch
          then splitOnSubstr' str' substr' (i, j+1)
          else splitOnSubstr' (drop (i+1) str) substr (i+1, i+1)
        splitOnSubstr' (ch':str') [] (i, j) = (take i str, drop (j) str)


-- Removes every occurrence of ' ' from a given string.
-- e.g. removeSpaces "ab c   d  e" = "abcde".
-- Note that this does not remove whitespace (e.g. \t) in general.
removeSpaces [] = []
removeSpaces (ch:str) =
  if ch == ' '
  then removeSpaces str
  else ch : removeSpaces str


-- Removes everything after an occurrence of "--" in a given string.
-- e.g. removeComment "this is not a comment-- this is a comment" = "this is not a comment".
{- removeComment str = removeComment' str ""
    where
        removeComment' [] no_com_str = no_com_str
        removeComment' ('-':'-':_) no_com_str = no_com_str
        removeComment' (ch:str') no_com_str = removeComment' str' (no_com_str ++ [ch])
-}


-- Wraps a string in quotes as long as that string is not a special symbol.
-- e.g. addQuotes "blah blah blah" = "\"blah blah blah\""
addQuotes str =
  -- If the given string is a reserved symbol, then do nothing.
  if str' == epsilon_sym || str' == "_"
  then str'
  -- Otherwise, wrap the string in quotes.
  else "\"" ++ str' ++ "\"" -- 
  where str' = removeSpaces str

------------------------------------------------------------------------------------------------------------------------------------------

-- Input lines are strings of the form "a,b,c->d,e" (where a, b, c, d, and e
-- should be thought of as substrings representing individual variables rather
-- than the literal characters 'a', 'b', .., 'e'), so we split on "->" to
-- obtain "a,b,c" and "d,e". Then split each of these strings on
-- the commas to obtain the strings a, b, c, d, and e. 
edgeComponents input = ec' (map (addQuotes.removeSpaces) [a,b,c,d,e])
    where
        (abc, de) = splitOnSubstr input "->"
        (a, bc) = splitOnCh abc ',' -- (state1, _)
        (b, c) = splitOnCh bc ',' -- (input, pop)
        (d, e) = splitOnCh de ',' -- (push, state2)
        ec' [a',b',c',d',e'] = (a',b',c',d',e') -- Shortcut for applying removeSpaces without writing it five times.


-- Formats recursive calls to the dpda machine for each transition rule.
base_line (state, (input, input'), (pop, push), next_state) =
    "    scan' " ++ input ++ " " ++ pop ++ " " ++ state 
    ++ " = scan' " ++ input' ++ " " ++ push ++ " " ++ next_state


-- Formats the dpda-input for a given transition rule into
-- an equivalent representation for the generated haskell code.
formInput :: String -> (String, String)
formInput "[]"  = ("[]", "[]")
formInput "_" = ("input", "input")
formInput input = ("(" ++ input ++ ":input)", "input")


-- Formats the dpda-stack operations for a given transition rule
-- into an equivalent representation for the generated haskell code.
formStack :: (String, String) -> (String, String)
formStack ("[]", "[]") = ("[]", "[]")
formStack ("[]", "_") = ("[]", "[]")
formStack ("_", "[]") = ("stack",  "[]")
formStack ("_", "_") = ("stack",  "stack")

formStack (pop, "[]") = ("(" ++ pop ++ ": stack)", "[]")
formStack (pop, "_") = ("(" ++ pop ++ ": stack)", "stack")

formStack ("[]", push) = ("[]", "[]")
formStack ("_", push) = ("stack", "(" ++ push ++ ":stack)")

formStack (pop, push) = ("(" ++ pop ++ ":stack)", "(" ++ push ++ ":stack)")


-- Builds source lines in haskell corresponding to source lines in the DPDA source file ()
buildLine (state1, input, pop, push, state2) = base_line (state1, formInput input, formStack (pop, push), state2)

----------------------------------------------------------------------------------------------------

-- Breaks up the source code for a DPDA.
-- Returns a triple containing the names of the initial and final states, and the transition rules.
-- The first line contains a single integer representing the name of the initial state.
-- The second line contains a single integer representing the name of the final state.
-- Remaining lines are rules the form "a,b,c->d,e" which can be read as:
--    At state a, if the head of the input is b and the top of the stack is c, then pop c,
--    push d, and go to state e.
readSource source_file_name = do
  handle <- openFile pda_src_file ReadMode -- Open the source file
  initial_state <- hGetLine handle -- Read the first line, which specifies the name of the initial state.
  final_state <- hGetLine handle -- Read the second line, which specifies the name of the final (accepting) state.
  rest <- hGetContents handle -- Read the remaining lines, which specify the rules (transitions) for the DPDA.
  return (initial_state, final_state, lines rest)

-- Compiles the DPDA program given the pieces returned by readSource.
compile startstate finalstate source_lines = [header_comment] ++ pda_result_type ++ ["\n"] ++ (output_base_fst startstate) ++ generated ++ (output_base_lst finalstate) ++ output_additional_functions
  where generated = map buildLine (map edgeComponents source_lines)

-- Reads a DPDA source file with readSource, and builds an equivalent haskell program.
main = do
  source <- readSource pda_src_file
  createAndWriteFile pda_dest_file $ concatMap (\s -> s ++ "\n") (compile (fst_3 source) (snd_3 source) (trd_3 source))
