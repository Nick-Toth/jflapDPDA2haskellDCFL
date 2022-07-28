-- Test:
main = putStrLn (if (test_accept == []) && (test_reject == []) then "test passed!" else "test failed!")
  where accept_these = [["([", "])"], ["([","])","([","])"], ["([","([","])","])"], ["([","([","])","([","])","])"], ["([","])","([","([","])","])"], ["([","])","([","([","([","])","])","])","([","])","([","])"], ["([","])","([","])","([","([","])","])","([","])","([","([","])","])","([","])"]]
        reject_these = [["(",")"], ["("], [")"], ["([",")"], ["(","])"], ["(["], ["])"], ["()","()"], ["([",")(","])"], ["(",")","(",")","(",")"] ]
        test_accept = filter (\x -> x /= Accept) (map scan accept_these)
        test_reject = filter (\x -> x /= Reject) (map scan reject_these)


-- This code was automatically generated. 


-- This class represents the results of the recognizer.
data PDA_Result = Accept | Reject deriving Eq
instance Show PDA_Result
  where
    show Accept = "Accept"
    show Reject = "Reject"


-- This is the scanner that you should use if your JFLAP DPDA was written in multi
-- character mode. To use it, apply this function to a list of strings, where each string
-- in the list represents a symbol in the language. For example, if your JFLAP DPDA is
-- written to recognize the language (ab)^k(cd)^k, then you can recognize "abcd" by
-- writing scan ["ab", "cd"], or "ababcdcd" by writing ["ab", "ab", "cd", "cd"], etc.
scan :: [String] -> PDA_Result
scan input = scan' input [] "0"
  where
    scan' input stack "0" = scan' input ("$":stack) "1"
    scan' ("([":input) stack "1" = scan' input ("([":stack) "1"
    scan' ("])":input) ("([": stack) "1" = scan' input stack "2"
    scan' ("([":input) stack "2" = scan' input ("([":stack) "1"
    scan' ("])":input) ("([": stack) "2" = scan' input stack "2"
    scan' input ("$": stack) "2" = scan' input stack "3"
    scan' [] [] "3" = Accept
    scan' _ _ _ = Reject


-- This is the scanner that you should use if your JFLAP DPDA was written in single
-- character mode. To use it, simply apply this function to the strings that you want
-- to recognize. For example, if your JFLAP DPDA is written to recognize the language
-- a^kb^k, and you want to recognize "ab", then simply write scan_char_sequence "ab".
scan_char_sequence :: String -> PDA_Result
scan_char_sequence str = scan $ break_string str
  where break_string = map (\ch -> [ch])

