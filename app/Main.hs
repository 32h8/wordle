{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Words
import Test.QuickCheck

data Color = Green | Yellow | Gray deriving (Show, Eq)
type Feedback = [(Char, Color)]
type WordleState = [Feedback]

isGreen :: Color -> Bool
isGreen Green = True
isGreen _     = False 

isYellow :: Color -> Bool
isYellow Yellow = True 
isYellow _      = False

isGray :: Color -> Bool
isGray Gray = True
isGray _    = False

matchGreen :: Feedback -> String -> Bool
matchGreen feedback w =  
    all (\((a,_),b) -> a == b)
    $ filter (isGreen . snd . fst) 
    $ zip feedback w

matchNonGreen :: Feedback -> String -> Bool 
matchNonGreen feedback w =
    all (\((a,_),b) -> a /= b)
    $ filter (not . isGreen . snd . fst) 
    $ zip feedback w

yellows :: Feedback -> String
yellows feedback = map fst $ filter (isYellow . snd) feedback

-- in function notGreens we assume input word matches green letters 
-- otherwise it will be filtered out by function matchGreen
notGreens :: Feedback -> String -> String
notGreens feedback w = map snd $ filter (not . isGreen . fst) $ zip (map snd feedback) w

hasAllYellows :: Feedback -> String -> Bool
hasAllYellows feedback w = all (flip elem w') $ yellows feedback
    where
        w' = notGreens feedback w

grays :: Feedback -> String
grays feedback = map fst $ filter (isGray . snd) feedback

noGrays :: Feedback -> String -> Bool 
noGrays feedback w = 
    all (not . flip elem w') 
    $ filter (not . flip elem ys) 
    $ grays feedback
    where
        ys = yellows feedback
        w' = notGreens feedback w

match :: Feedback -> String -> Bool
match feedback w = 
    matchGreen feedback w
    && matchNonGreen feedback w
    && hasAllYellows feedback w
    && noGrays feedback w

matchAll :: WordleState -> String -> Bool
matchAll feedbacks w = all (flip match w) feedbacks

nextGuess :: [String] -> WordleState -> [String]
nextGuess candidates feedbacks = filter (matchAll feedbacks) candidates

colorFromChar :: Char -> Color
colorFromChar c = case c of 
    'g' -> Green
    'y' -> Yellow
    'x' -> Gray 

colorsFromString :: String -> [Color]
colorsFromString str = map colorFromChar $ take 5 str

validColorString :: String -> Bool 
validColorString s = length s >= 5 && (all (flip elem "gyx") $ take 5 s)

step :: [String] -> WordleState -> IO ()
step candidates fs = do
    let gs = nextGuess candidates fs
    if null gs
        then putStrLn "could not find matching word"
        else do
            let g = head gs
            putStrLn $ "guess: " ++ g 
            putStrLn "type feedback colors (or press enter to quit): "
            colorsStr <- getLine
            if null colorsStr
            then putStrLn "End."
            else if not $ validColorString colorsStr
                then putStrLn "failed to parse colors"
                else do
                    let colors = colorsFromString colorsStr
                    let feedback = zip g colors :: Feedback
                    if all isGreen colors
                        then putStrLn "End."
                        else step candidates (feedback : fs)

allWords :: [String]
allWords = guessWords ++ finalWords

main :: IO ()
main = do
    let ws = allWords
    putStrLn "instructions for typing colors: "
    putStrLn "type \"gxxxx\" for following color sequence: Green Gray Gray Gray Gray"
    putStrLn "type \"gyxxx\" for following color sequence: Green Yellow Gray Gray Gray"
    putStrLn ""
    step ws []

-- function for generating feedback for given word based on answer
giveFeedback :: String -> String -> Feedback
giveFeedback guess answer = 
    zipWith3 comp [0..] guess answer
    where
        count :: Char -> String -> Int
        count x w = length $ filter (== x) w

        nonGreensInAnswer :: String
        nonGreensInAnswer = 
            map fst 
            $ filter snd
            $ zip answer 
            $ zipWith (/=) guess answer 

        comp :: Int -> Char -> Char -> (Char, Color)
        comp i g a
            | g == a = (a, Green)
            | notElem g answer = (g, Gray)
            | notElem g nonGreensInAnswer = (g, Gray)
            | elem g nonGreensInAnswer = -- will be Yellow or Gray
               if count g answer == 1 && count g guess == 1
                then (g, Yellow)
                else let 
                    gInGuessCount :: Int
                    gInGuessCount = 
                        count g 
                        $ map fst
                        $ filter snd 
                        $ take i 
                        $ zip guess 
                        $ zipWith (/=) guess answer
                    
                    gInAnswerCount :: Int
                    gInAnswerCount = count g nonGreensInAnswer

                    in (g, if gInGuessCount < gInAnswerCount then Yellow else Gray)

-- for QuickCheck
newtype WordleWord = WordleWord String deriving (Show, Eq)

instance Arbitrary WordleWord where
    arbitrary = elements $ map WordleWord finalWords

allGreen :: Feedback -> Bool
allGreen xs = all isGreen $ map snd xs

solve :: Int -> [String] -> WordleState -> String -> (Bool, Int)
solve i canidates feedbacks answer = 
    case nextGuess canidates feedbacks of 
        [] -> (False, i)
        (guess : newCandidates) ->
            let newFeedback = giveFeedback guess answer
            in if allGreen newFeedback
                then (True, i)
                else solve (i+1) newCandidates (newFeedback : feedbacks) answer

prop_solve :: WordleWord -> Property
prop_solve (WordleWord answer) = 
    let (b, i) = solve 1 allWords [] answer
    in collect i b 