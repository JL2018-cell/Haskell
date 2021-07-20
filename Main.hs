{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Combinators
import Parser
import Regex

import System.Timeout
import Data.List
import Data.Maybe
import Data.Char
import Control.Exception
import Control.Applicative hiding (optional)
import Control.Monad
import Control.Arrow (first, second)
import Control.DeepSeq

import Text.Printf

import Parser
import Combinators

simplifyResult :: Ord a => [(a, String)] -> [(a, String)]
simplifyResult = sort . nub

parseResultMatch :: Ord a => [(a, String)] -> [(a, String)] -> Bool
parseResultMatch r1 r2 = simplifyResult r1 == simplifyResult r2

tryRun :: a -> IO (Either SomeException a)
tryRun = try . evaluate

tryIO :: IO a -> IO (Either SomeException a)
tryIO = try

data TestError a
  -- | 'a' is the error output.
  = TSimpleFailure a (Maybe String)
  -- | First 'a' is the error output.
  --   Second 'a' is the expected answer.
  | TMismatchResult a a
  | TTimeout Int
  | TException SomeException
  deriving Show

data TestResult a
  = TSuccess
  | TFailure (TestError a)
  deriving Show

data Annotated a = a :? String

instance Show (Annotated a) where
  show (_ :? s) = s

successful :: TestResult a -> Bool
successful = \case
  TSuccess -> True
  _        -> False

data Judged a
  = JSuccess
  | JMismatch a
  | JFailReason String
  | JFail

data TestCase a b c
  = TestCase
  { input :: a
  -- | Maximum time in millisecond to run the test
  , timeLimit :: Int
  -- | How to execute the test casse
  , execute :: b -> IO c
  -- | Input 'b' is the output of the tested program.
  --   returns 'Right' if output is correct, or 'Left' if it's not correct and
  --   an optional expected answer may be attached.
  , judge :: c -> Judged c
  -- | Weight among all the test cases of one question
  , caseWeight :: Int
  -- | A brief description of the case
  , caseComment :: String
  }

(<#>) :: TestCase a b c -> String -> TestCase a b c
TestCase{..} <#> comment = TestCase{caseComment = comment, ..}

(<!>) :: TestCase a b c -> Int -> TestCase a b c
TestCase{..} <!> weight = TestCase{caseWeight = weight, ..}

infixl 6 <!>, <#>

class Convertible a b | a -> b where
  convert :: a -> b

instance Convertible (Annotated a) a where
  convert (a :? _) = a

instance (Convertible a1 a2, Convertible b1 b2)
  => Convertible (a1, b1) (a2, b2) where
  convert (a, b) = (convert a, convert b)

instance (Convertible a1 a2, Convertible b1 b2, Convertible c1 c2)
  => Convertible (a1, b1, c1) (a2, b2, c2) where
  convert (a, b, c) = (convert a, convert b, convert c)

instance (Convertible a1 a2, Convertible b1 b2)
  => Convertible (Either a1 b1) (Either a2 b2) where
  convert (Left a) = Left (convert a)
  convert (Right b) = Right (convert b)

instance (Convertible a b) => Convertible [a] [b] where
  convert = fmap convert

instance {-# OVERLAPPABLE #-} a ~ b => Convertible a b where
  convert = id

runTestCaseIO
  :: Convertible a b
  => NFData c
  => TestCase a b c
  -> IO (TestResult c)
runTestCaseIO TestCase{..} =
  interpretResult <$>
    tryIO (timeout (timeLimit * 1000) $ execute (convert input) >>= evaluate . force)
  where
    interpretResult = \case
      Right Nothing -> TFailure (TTimeout timeLimit)
      (Left e) -> TFailure $ TException e
      (Right (Just c)) -> case judge c of
        JMismatch sample -> (TFailure . TMismatchResult c) sample
        JFailReason s -> TFailure $ TSimpleFailure c (Just s)
        JFail -> TFailure $ TSimpleFailure c Nothing
        JSuccess -> TSuccess

runTestCasesIO
  :: Convertible a b => NFData c
  => [TestCase a b c] -> IO [TestResult c]
runTestCasesIO = traverse runTestCaseIO

data PackTest where
  PackTest
    :: forall a b c. (Show a, Show c, Convertible a b, NFData c)
    => TestCase a b c -> PackTest

infixr 5 <:>

(<:>)
  :: (Show a, Convertible a b, Show c, NFData c)
  => TestCase a b c -> [PackTest] -> [PackTest]
t <:> ts = PackTest t : ts

packedTestWeight :: PackTest -> Int
packedTestWeight (PackTest t) = caseWeight t

packedTestComment :: PackTest -> String
packedTestComment (PackTest t) = caseComment t

packedTestTimeLimit :: PackTest -> Int
packedTestTimeLimit (PackTest t) = timeLimit t

data Problem
  = Problem
  { questionId :: Int
  , totalScore :: Int
  , testCases :: [PackTest]
  }

data TestedCase where
  TestedCase
    :: forall a b c. (Show a, Show c, Convertible a b, NFData c) =>
    TestCase a b c -> TestResult c -> TestedCase

testedTest :: TestedCase -> PackTest
testedTest (TestedCase t _) = PackTest t

testedSuccessful :: TestedCase -> Bool
testedSuccessful (TestedCase _ r) = successful r

runPackTest :: PackTest -> IO TestedCase
runPackTest (PackTest t) = TestedCase t <$> runTestCaseIO t

runPackTests :: [PackTest] -> IO [TestedCase]
runPackTests = traverse runPackTest

data ProblemSummary
  = ProblemSummary
  { problem :: Problem
  , testResults :: [TestedCase]
  , problemScore :: Int
  }

summaryTotalScore :: ProblemSummary -> Int
summaryTotalScore = totalScore . problem

-- | Alternate intercalate that ignores empty list
intercalate' :: [a] -> [[a]] -> [a]
intercalate' sep = intercalate sep . filter (not . null)

runProblem :: Problem -> IO ProblemSummary
runProblem Problem{..} = do
  testResults <- runPackTests testCases
  let problem = Problem {..}
  let totalWeight = sum $ packedTestWeight <$> testCases
      passedCases =
        testedTest <$> filter testedSuccessful testResults
      passedWeight = sum $ packedTestWeight <$> passedCases
      problemScore = totalScore * passedWeight `div` totalWeight
  return ProblemSummary{..}

formatProblemSummary
  :: ([TestedCase] -> String)
  -> ProblemSummary
  -> String
formatProblemSummary formatter ProblemSummary{..} =
  let Problem{..} = problem
  in intercalate' "\n"
  [ printf "Problem %d: %d/%d" questionId problemScore totalScore
  , formatter testResults
  ]

genShowProblemSummary
  :: (TestedCase -> String)
  -> String
  -> ProblemSummary -> String
genShowProblemSummary showCase separator =
  formatProblemSummary $ intercalate separator . (showCase <$>)

basicShowCase
  :: TestedCase -> String
basicShowCase (TestedCase TestCase{..} result) =
  printf "Input: %s. %s.%s%s" (show input) passOrFail comment failMessage
  where
    passOrFail = if successful result then "PASS" else "FAIL"
    comment = if null caseComment
      then "\n"
      else " # " ++ caseComment ++ "\n"
    failMessage = case result of
      TSuccess -> ""
      TFailure e -> case e of
        TSimpleFailure o m -> printf "    OUTPUT: %s\n%s" (show o) $ case m of
          Nothing -> ""
          Just e -> printf "POSSIBLE FAIL REASON: %s\n" e
        TMismatchResult o expected ->
          printf "    OUTPUT: %s\n    EXPECTED: %s\n"
            (show o) (show expected)
        TTimeout n ->
          printf "    TIMEOUT for %.2f sec (possibly infinite loop?)\n"
            (fromIntegral n / 1000 :: Double)
        TException e -> printf "    RUNTIME EXCEPTION: %s" $ show e

showScoreOnly :: ProblemSummary -> String
showScoreOnly = formatProblemSummary (const "")

showProblemSummary
  :: ProblemSummary -> String
showProblemSummary = genShowProblemSummary basicShowCase ""

parsingResultsMatch
  :: Ord a
  => [(a, String)] -> [(a, String)] -> Judged [(a, String)]
parsingResultsMatch expected output = if parseResultMatch expected output
  then JSuccess
  else JMismatch $ simplifyResult expected

genParserTestCase
  :: (Ord a, Convertible p p')
  => (p' -> Parser a) -> (p, String) -> [(a, String)]
  -> TestCase (p, String) (p', String) [(a, String)]
genParserTestCase f (args, s) expected =
  let timeLimit = 1000
      execute (p', s) = return $ runParser (f p') s
      input = (args, s)
      judge = parsingResultsMatch expected
      caseWeight = 1
      caseComment = ""
  in TestCase {..}

parserTestCase
  :: Ord a
  => Parser a -> String -> [(a, String)]
  -> TestCase String String [(a, String)]
parserTestCase p input expected =
  let timeLimit = 1000
      execute = return . runParser p
      judge = parsingResultsMatch expected
      caseWeight = 1
      caseComment = ""
  in  TestCase{..}


q1 :: Problem
q1 = Problem {..}
  where
    questionId = 1
    totalScore = 16
    testCases
      =   ""    `parseTo` []             <!> 2 <#> "Empty string should fail"
      <:> "a"   `parseTo` [('a', "a")]   <!> 1 <#> "Single character"
      <:> "bcd" `parseTo` [('b', "bcd")] <!> 2 <#> "Multiple characters"
      <:> []
    parseTo = parserTestCase lookAhead

q2 :: Problem
q2 = Problem{..}
  where
    questionId = 2
    totalScore = 13
    testCases
      =   (eof :? "eof", "")    `parseTo` [(Just (), "")]
        <!> 3 <#> "Optional eof Succeeds"
      <:> (eof :? "eof", "abc") `parseTo` [(Nothing, "abc")]
        <!> 3 <#> "Optional eof fails"
      <:> (some (char 'a') :? "some (char 'a')", "bcd")
        `parseTo` [(Nothing, "bcd")]
        <!> 2 <#> "There are no 'a's"
      <:> (some (char 'a') :? "some (char 'a')", "aabcd")
        `parseTo` [(Just "a", "abcd"), (Just "aa", "bcd")]
        <!> 1 <#> "There are many 'a's"
      <:> []

    parseTo
      -- :: Ord a
      -- => (Annotated (Parser a), String) -> [(Maybe a, String)]
      -- -> TestCase (Annotated (Parser a), String) (Parser a, String) [(Maybe a, String)]
      :: _ => (Annotated (Parser a), String) -> _
    parseTo = genParserTestCase optional

q3 :: Problem
q3 = Problem{..}
  where
    questionId = 3
    totalScore = 16
    testCases
      =   ([ char 'a' :? "char 'a'"
           , char 'b' :? "char 'b'"
           , char 'c' :? "char 'c'"], "abc") `parseTo` [('a', "bc")]
            <!> 2 <#> "Only char 'a' succeeds"
      <:> ([ string "a" :? "string \"a\""
           , string "ab" :? "string \"ab\""
           , string "abcd" :? "string \"abcd\""], "abc")
           `parseTo` [("a", "bc"), ("ab", "c")]
            <!> 2 <#> "Multiple choices succeed"
      <:> ([], "abcd") `parseTo` ([] :: [((), String)])
            <!> 1 <#> "No parser provided should fail"
      <:> []

    parseTo :: _ => ([Annotated (Parser a)], String) -> _
    parseTo = genParserTestCase choose

q4 :: Problem
q4 = Problem{..}
  where
    questionId = 4
    totalScore = 10
    testCases
      =   ((char 'a' :? "char 'a'", char 'b' :? "char 'b'"), "abc")
          `parseTo` [('a', "bc")]
            <!> 3 <#> "'a' is followed by 'b'"
      <:> ((char 'a' :? "char 'a'", char 'c' :? "char 'c'"), "abc")
          `parseTo` []
            <!> 3 <#> "'a' is not followed by 'b'"
      <:> ((many (char 'a') :? "many 'a'", eof :? "eof"), "aaa")
          `parseTo` [("aaa", "")]
            <!> 1 <#> "Only one case is followed by eof"
      <:> ((many (satisfy isAlpha) :? "many (satisfy isAlpha)"
          , char 'a' :? "char 'a'"), "acdaea12a")
          `parseTo` [("", "acdaea12a"), ("acd", "aea12a"), ("acdae", "a12a")]
            <!> 1 <#> "Characters followed by 'a'"
      -- <:> ((many (satisfy isAlphaNum) :? "many (satisfy isAlphaNum)"
      --     , satisfy isAlpha :? "satisfy isAlpha"), "a12b3cd4" )
      --     `parseTo` [ ("", "a12b3cd4"), ("a12", "b3cd4"), ("a12b3", "cd4")
      --               , ("a12b3c", "d4")]
      --       <!> 1 <#> "Characters followed by letters"
      <:> []

    parseTo :: _ => ((Annotated (Parser a), Annotated (Parser b)), String) -> _
    parseTo = genParserTestCase (uncurry followedBy)

q5 :: Problem
q5 = Problem{..}
  where
    questionId = 5
    totalScore = 10
    testCases
      =   ((char 'a' :? "char 'a'", char 'b' :? "char 'b'"), "aaabc")
          `parseTo` [("aaa", "c")]
            <!> 1 <#> "many 'a' stopped by a 'b'"
      <:> ((char 'a' :? "char 'a'", char 'b' :? "char 'b'"), "bcc")
          `parseTo` [([], "cc")]
            <!> 2 <#> "start by 'b'"
      <:> ((char 'a' :? "char 'a'", char 'b' :? "char 'b'"), "aaaa")
          `parseTo` []
            <!> 2 <#> "none of the 'a's is stopped by 'b'"
      <:> ( (char 'a' :? "char 'a'", satisfy isAlpha :? "satisfy isAlpha")
          , "aabc")
          `parseTo` [("", "abc"), ("a", "bc"), ("aa", "c")]
            <!> 2 <#> "A lot of 'a's are followed by letter"
      <:> (( satisfy isAlpha :? "satisfy isAlpha"
           , some (satisfy isAlpha) :? "some (satisfy isAlpha)"), "abc")
          `parseTo` [ ("", "bc"), ("", "c"), ("", ""), ("a", "c"), ("a", "")
                    , ("ab", "")]
            <!> 3 <#> "A lot of letter followed by a lot of letters"
      <:> []


    parseTo :: _ => ((Annotated (Parser a), Annotated (Parser b)), String) -> _
    parseTo = genParserTestCase (uncurry manyUntil)

q6 :: Problem
q6 = Problem {..}
  where
    questionId = 67
    totalScore = 20
    testCases
      =   "a"     `parseTo` Just "a"   <#> "One character"
      <:> "."     `parseTo` Just "."   <#> "One character"
      <:> "(a)"   `parseTo` Just "a"   <#> "One character"
      <:> "abc"   `parseTo` Just "abc" <#> "Basic Concatenation"
      <:> "(ab)c" `parseTo` Just "abc" <#> "Basic Concatenation"
      <:> "a(bc)" `parseTo` Just "abc" <#> "Basic Concatenation"
      <:> "(abc)" `parseTo` Just "abc" <#> "Basic Concatenation"
      <:> "a|b"     `parseTo` Just "a|b"   <#> "Basic Disjunction"
      <:> "a|b|c"   `parseTo` Just "a|b|c" <#> "Basic Disjunction"
      <:> "(a|b)|c" `parseTo` Just "a|b|c" <#> "Basic Disjunction"
      <:> "a|(b|cd)" `parseTo` Just "a|b|cd" <#> "Basic Disjunction"
      <:> "a+" `parseTo` Just "a+" <#> "Basic Postfix"
      <:> "(a+)*" `parseTo` Just "(a+)*" <#> "Basic Postfix"
      <:> "ab|cd"  `parseTo` Just "ab|cd"  <#> "Basic Priority (Concat & Or)"
      <:> "(ab)|cd" `parseTo` Just "ab|cd" <#> "Basic Priority (Concat & Or)"
      <:> "a(b|c)" `parseTo` Just "a(b|c)" <#> "Basic Priority (Concat & Or)"
      <:> "ab*"    `parseTo` Just "ab*"   <#> "Basic Priority (Concat & Postfix)"
      <:> "a(b+)"  `parseTo` Just "ab+"   <#> "Basic Priority (Concat & Postfix)"
      <:> "(ab)+"  `parseTo` Just "(ab)+" <#> "Basic Priority (Concat & Postfix)"
      <:> "a|b+"   `parseTo` Just "a|b+"  <#> "Basic Priority (Or & Postfix)"
      <:> "a|(b+)" `parseTo` Just "a|b+"  <#> "Basic Priority (Or & Postfix)"
      <:> "(a|b)+" `parseTo` Just "(a|b)+" <#> "Basic Priority (Or & Postfix)"
      <:> "ab?c|abc+" `parseTo` Just "ab?c|abc+" <!> 2 <#> "Complex Priority"
      <:> "(a(b?)c)|(ab(c+))" `parseTo` Just "ab?c|abc+" <!> 2 <#> "Complex Priority"
      <:> "a(b?c|(abc)+)" `parseTo` Just "a(b?c|(abc)+)" <!> 2 <#> "Complex Priority"
      <:> "(ab)?(c|abc)+" `parseTo` Just "(ab)?(c|abc)+" <!> 2 <#> "Complex Priority"
      <:> "ab?(c|a)|bc+" `parseTo` Just "ab?(c|a)|bc+" <!> 2 <#> "Complex Priority"
      <:> "((a))"       `parseTo` Just "a"     <#> "Basic Bracket Elim"
      <:> "(((a(b))))"  `parseTo` Just "ab"    <#> "Basic Bracket Elim"
      <:> "((a(bc)))"   `parseTo` Just "abc"   <#> "Basic Bracket Elim"
      <:> "(a+)(b*)"    `parseTo` Just "a+b*"  <#> "Basic Bracket Elim"
      <:> "(a+)|(b*)"   `parseTo` Just "a+|b*" <#> "Basic Bracket Elim"
      <:> "(ab)|((c)d)" `parseTo` Just "ab|cd" <#> "Basic Bracket Elim"
      <:> "(a+)(b?)*"   `parseTo` Just "a+(b?)*" <#> "Basic Bracket Elim"
      <:> "(a(bc))+|(de)" `parseTo` Just "(abc)+|de"
        <!> 2 <#> "Complex Bracket Elim"
      <:> "((ab)+c)|((abc|ab)+|c)" `parseTo` Just "(ab)+c|(abc|ab)+|c"
        <!> 2 <#> "Complex Bracket Elim"
      <:> "(a+)((a+)*)(b(cd+|e))" `parseTo` Just "a+(a+)*b(cd+|e)"
        <!> 2 <#> "Complex Bracket Elim"
      <:> "((a|(b(c|d))*)(ef+))" `parseTo` Just "(a|(b(c|d))*)ef+"
        <!> 2 <#> "Complex Bracket Elim"
      <:> "(a"     `parseTo` Nothing <#> "Basic Error (Mismatched Bracket)"
      <:> "a)"     `parseTo` Nothing <#> "Basic Error (Mismatched Bracket)"
      <:> "((a)"   `parseTo` Nothing <#> "Basic Error (Mismatched Bracket)"
      <:> "((a)))" `parseTo` Nothing <#> "Basic Error (Mismatched Bracket)"
      <:> "a++"    `parseTo` Nothing <#> "Basic Error (Consecutive Operators)"
      <:> "ab+*"   `parseTo` Nothing <#> "Basic Error (Consecutive Operators)"
      <:> "a||"    `parseTo` Nothing <#> "Basic Error (Consecutive Operators)"
      <:> "a||b"   `parseTo` Nothing <#> "Basic Error (Consecutive Operators)"
      <:> "+ab"    `parseTo` Nothing <#> "Basic Error (Misplaced Operators)"
      <:> "|ab"    `parseTo` Nothing <#> "Basic Error (Misplaced Operators)"
      <:> "a(|ab)" `parseTo` Nothing <#> "Basic Error (Misplaced Operators)"
      <:> "a(*ab)" `parseTo` Nothing <#> "Basic Error (Misplaced Operators)"
      <:> "(a())"  `parseTo` Nothing <#> "Basic Error (Empty Bracket)"
      <:> "()"     `parseTo` Nothing <#> "Basic Error (Empty Bracket)"
      <:> "(a+)(a+*)(b(cd+|e))" `parseTo` Nothing
        <!> 2 <#> "Slightly Complex Error"
      <:> "(ab(cd()?e))" `parseTo` Nothing
        <!> 2 <#> "Slightly Complex Error"
      <:> "ab(cd(d+d)?)e(+)" `parseTo` Nothing
        <!> 2 <#> "Slightly Complex Error"
      <:> []

    parseTo :: String -> Maybe String -> TestCase String String [(String, String)]
    parseTo rin mrout = TestCase{..}
      where
        input = rin
        timeLimit = 1000
        judge = case mrout of
          Nothing -> parsingResultsMatch []
          Just s -> parsingResultsMatch [(s, "")]
        caseWeight = 1
        caseComment = ""
        execute s = return $ first show <$> runParser regexP s

q8 :: Problem
q8 = Problem{..}
  where
    questionId = 8
    totalScore = 15
    testCases
      =   ("a", "abc")      `matchTo` [("a", "bc")]
            <#> "Only Matching Start (Single Character)"
      <:> ("abc", "abcd")    `matchTo` [("abc", "d")]
            <#> "Only Matching Start (Simple Concat)"
      <:> ("ab|cd", "abac") `matchTo` [("ab", "ac")]
            <#> "Only Matching Start (Simple Disjunction)"
      <:> ("ab|cd", "cdac") `matchTo` [("cd", "ac")]
            <#> "Only Matching Start (Simple Disjunction)"
      <:> ("ab|abc", "abcd") `matchTo` [("ab", "cd"), ("abc", "d")]
            <#> "Only Matching Start (Simple Disjunction)"
      <:> ("a+", "abc")     `matchTo` [("a", "bc")]
            <#> "Only Matching Start (Simple Plus)"
      <:> (".", "e") `matchTo` [("e", "")]
            <#> "Only Matching Start (Simple Dot)"
      <:> ("a*", "") `matchTo` [("", "")]
            <#> "Only Matching Start (Empty String)"
      <:> ("a?", "") `matchTo` [("", "")]
            <#> "Only Matching Start (Empty String)"
      <:> (".", "abc") `matchTo` [("a", "bc"), ("b", "c"), ("c", "")]
            <#> "Simple Dot"
      <:> ("a", "abacda") `matchTo` [("a", "bacda"), ("a", "cda"), ("a", "")]
            <#> "Single Character (Multiple Match)"
      <:> ("abc", "abcdabcd") `matchTo` [("abc", "dabcd"), ("abc", "d")]
            <#> "Concatenation (Multiple Match)"
      <:> ("ab|ba", "ababa") `matchTo`
            [("ab", "aba"), ("ba", "ba"), ("ab", "a"), ("ba", "")]
            <#> "Disjunction (Multiple Match)"
      <:> ("b+", "abbc") `matchTo` [("b", "bc"), ("bb", "c"), ("b", "c")]
            <#> "Plus (Multiple Match)"
      <:> ("b*", "abbc") `matchTo`
          [ ("", "abbc"), ("", "bbc"), ("b", "bc"), ("bb", "c")
          , ("", "bc"), ("b", "c"), ("", "c"), ("", "")]
            <#> "Plus (Multiple Match)"
      <:> ("b?", "abab") `matchTo`
          [ ("", "abab"), ("", "bab"), ("b", "ab")
          , ("", "ab"), ("", "b"), ("b", ""), ("", "")]
            <#> "Optional (Multiple Match)"
      <:> (".*" ,"ab") `matchTo`
          [("", "ab"), ("a", "b"), ("ab", ""), ("", "b"), ("b", ""), ("", "")]
            <#> "Dot Star Matches Everything"
      <:> ("ab?bc", "abc") `matchTo` [("abc", "")]
            <#> "Optional Doesn't Match"
      <:> ("(abc)+", "abcabca") `matchTo`
          [("abc", "abca"), ("abcabc", "a"), ("abc", "a")]
            <#> "Multiple Concatenation"
      <:> ("(ab)+|(ba)+", "ababa") `matchTo`
          [ ("ab", "aba"), ("abab", "a"), ("ab", "a")
          , ("ba", "ba"), ("baba", ""), ("ba", "")]
            <#> "Disjunction of Multiple Concat"
      <:> ("(ab|ba)+", "aababba") `matchTo`
          [ ("ab", "abba"), ("abab", "ba"), ("ababba", "")
          , ("ba", "bba"), ("ab", "ba"), ("abba", ""), ("ba", "")]
            <#> "Multiple Disjuction of Concat"
      <:> ("(ab?c)+", "abcacdacab") `matchTo`
          [("abc", "acdacab"), ("abcac", "dacab"), ("ac", "dacab"), ("ac", "ab")]
            <#> "Multiple Optional Concat"
      <:> ("((c|a)b?(a|c))+", "abcacdaccab") `matchTo`
           [ ("abc", "acdaccab"), ("abcac", "daccab"), ("ca", "cdaccab")
           , ("ac", "daccab"), ("ac", "cab"), ("acca", "b")
           , ("cc", "ab"), ("ca", "b")]
            <!> 2 <#> "Complex Regex 1"
      <:> ("(a|ab+)+c+", "abbaabbbcc") `matchTo`
          [ ("abbaabbbc", "c"), ("abbaabbbcc", ""), ("aabbbc", "c")
          , ("aabbbcc", ""), ("abbbc", "c"), ("abbbcc", "")]
            <!> 2 <#> "Complex Regex 2"
      <:> ("(c(a(a|b)*b)?c)+", "cabbabcccabc") `matchTo`
          [ ("cabc", ""), ("cc", "abc"), ("cccabc", ""), ("cc", "cabc")
          , ("cabbabccc", "abc"), ("cabbabc", "ccabc")]
            <!> 2 <#> "Complex Regex 3"
      <:> []


    matchTo :: (String, String) -> [(String, String)] -> _
    matchTo (r, s) res = TestCase{..}
      where
        input = (r, s)
        timeLimit = 1000
        judge = parsingResultsMatch res
        caseWeight = 1
        caseComment = ""
        execute (r, s) = case filter (null . snd) (runParser regexP r) of
          [] -> throwIO $
            AssertionFailed $
              printf "%s should be parsed to a valid regular expression\n" r
          (r, _) : _ -> return $ runParser (matcher r) s


summarySummaries :: [ProblemSummary] -> String
summarySummaries sms = intercalate "," $ show <$> sum scores : scores
  where scores = fmap problemScore sms

main :: IO ()
main = do
  forM [q1, q2, q3, q4, q5, q6, q8] runProblem >>= \ps -> do
    putStrLn $ summarySummaries ps
    mapM_ (putStrLn . showProblemSummary) ps
