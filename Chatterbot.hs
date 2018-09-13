module Chatterbot where
import Utilities
import System.Random
import Data.Char

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind brain = do
  -- get a random float value as r
  r <- randomIO:: IO Float
  -- map2 (id, pick r):
  --  The (id , pick r) function will return the phrase in the [(Phrase,
  --  [Phrase])] Botbrain structure and pick the r'th (frac) line from the
  --  possible answers in the [Phrase] part of the tuple.
  -- map ( ... ) brain:
  --  maps the above map2 function over the tuple list in the bot brain,
  --  resulting in a tuple list with all the possible transformations and one
  --  of the possible answers.
  -- rulesApply $ ..:
  --  results in a function loaded with the different transformations from the
  --  above step that needs a suitable second string to return either a
  --  reflected transformed string, or a empty list [].
  return $ rulesApply $ map (map2 (id, pick r)) brain

rulesApply :: [PhrasePair] -> Phrase -> Phrase
  -- If the transformationsApply fuction returns something, apply the id
  -- function to the result (does nothing) and return that. Otherwise return an
  -- empty list if the result is Nothing.
rulesApply = (maybe [] id .) . transformationsApply "*" reflect

reflect :: Phrase -> Phrase
  -- Flipping the lookup function result in a function that takes a tuplelist
  -- first and a key second and returns the value for for the given key if the
  -- (key,value) tuple is present in the tuple list.
  -- The try command is a wrapper around the maybe function and takes a value
  -- and a function. If the application of the function on the value results in
  -- something, this is returned. Otherwise the supplied value is returned unchanged.
  -- Use map to map the "try to reflect the word, otherwise return the word" -
  -- function over list of words (Phrase).
reflect = map $ try $ flip lookup reflections

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
-- Results in a function that takes a string and makes it lowercase (char by
-- char) and then returns a boolean describing if the translated string is
-- equal to "quit" or not.
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
-- Simple wrapper of the unwords command.
present = unwords

prepare :: String -> Phrase
-- elem a [a]: check if elem a is in a list of a's.
-- flip elem -> elem [a] a, same as above.
-- not . f: apply f and use the result as input for not, inverting it.
-- filter bf: use bool-function to pick items from a list.
-- map toLower . ff : apply ff and use  map toLower over the filtered list
-- words . lcff : apply the llcf functino and split the resulting string into
--               list of words.
-- reduce . wslcff : apply wslcff function and reduce the result.
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
-- First map maps over the outer list of tuples [(t1, t2)]
-- second map2 f1 f2 maps f1 over String and f2 over [String] in (String, [String])
-- the last map maps a function that sets the string to lowercase on a char to
-- char basis and then splits the converted string into words (= Phrase).
rulesCompile = map $ map2 (words . map toLower, map words)

--------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
-- transformationsApply "*" id r: results in a function that takes a String and
-- applies a transformation from the reductions data structure to the supplied
-- String. The above function is then used as the function part of the try
-- function. The resulting function is then used as the function part of the
-- next function, fix. By using the try-wrapped function with the fix function, the
-- reduction is repeated until it no longer changes the supplied string.
reductionsApply = fix . try . transformationsApply "*" id


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
-- simple pattern matching. If the function gets a empty list as the third
-- parameter, return the second parameter.
-- Otherwise, iterate over the items in the second parameter, comparing int to
-- the first parameter (the wildcard char). If the current argument matches the
-- supplied wildcard, append the substitution (third argument) to the result
-- and call the substitute function recursively. If the current element of the
-- second argument does not match the wildcad, append the current element and
-- call the substitute recursively.
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ b []  = b
substitute a (b:bs) c
  | a == b = c ++ substitute a bs c
  | otherwise = [b] ++ substitute a bs c
substitute _ _ _  = []



-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
-- If both the supplied lists have been consumed at the same time (both were
-- the same length) with no match evaluate to Just [].
match _ [] [] = Just []
-- If any of the second or third list is consumed before the other, evaluate to
-- Nothing.
match _ [] _ = Nothing
match _ _ [] = Nothing
-- Function takes wildcard and two strings which are split as head and tail.
match n (x:xs) (y:ys)
  -- n == x -> current item x is equal to the supplied wildcard.
  -- orElse v1 v2: if v1 is defined, evaluate to v1. If v1 is Nothing, evaluate to v2.
  | n == x = orElse (singleWildcardMatch (x:xs) (y:ys)) (longerWildcardMatch (x:xs) (y:ys))
  -- If the two heads of the lists match, call self recursively with the
  -- wildcard and the tails of the lists.
  | x == y = match n xs ys -- continue through the lists.
  -- If the head of first list match neither the wildcard or the head of the
  -- second list evaluate to Nothing.
  | otherwise = Nothing


-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
-- (const [x]) results in a function that takes an argument but always returns
-- [x] regardless of what the argument is, the match (..) is called to keep on
-- iterating over the list.
-- mmap takes a function and a Maybe value and evaluates to Nothing if the
-- value was Nothing. If the value was Something, it applies the function to it and
-- evaluates to the result of the application.
-- Separate the to supplied lists into head and tail segments and iterate over
-- them, the match (...) is called to continue the iteration.
singleWildcardMatch (wc:ps) (x:xs) = mmap (const [x]) (match wc ps xs)


-- Consuming the whole second list evaluates to Nothing.
longerWildcardMatch _ [] = Nothing
-- match wc (wc:ps) xs consumes the list supplied as the second argument but
-- keeps the list supplied as the first argument intact.
-- (x:) results in a function that takes a [x] as an argument and prepends x to
-- it.
-- mmap (x:) value: will evaluate to value prepended with x if the value is
-- anything other than Nothing, otherwise it evaluates to Nothing.
longerWildcardMatch (wc:ps) (x:xs) = mmap (x:) (match wc (wc:ps) xs)


-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
-- match wc t1 xs: evaluates to either Nothing or [a].
-- mmap f $ v: if v is Something apply f, otherwise evaluate to Nothing.
-- mmap (substitute wc t2) v1: if v1 is something, evaluate to substitute wc t2 v1,
-- otherwise evaluate to Nothing.
transformationApply wc f xs (t1, t2) = mmap (substitute wc t2)  (mmap f $ match wc t1 xs)

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
-- If the list supplied as the third argument is consumed evaluate to Nothing.
-- Evaluate transformationApply applied on the first element of the list
-- supplied as the 3'rd attribute. If it evaluates to Nothing call self
-- recursively with the tail part of the 3'rd attribute, otherwise evaluate to
-- the result of the transformationApply.
transformationsApply = ((((foldr1 orElse .) .) . flip ((.) . flip map)) .) . transformationApply
