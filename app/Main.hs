module Main where

import Words

data Color = Green | Yellow | Gray
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

matchYellow :: Feedback -> String -> Bool
matchYellow feedback w =
    all (\((a,_),b) -> a /= b)
    $ filter (isYellow . snd . fst) 
    $ zip feedback w

notGreens :: Feedback -> String -> String
notGreens feedback w = map snd $ filter (not . isGreen . fst) $ zip (map snd feedback) w

noGrays :: Feedback -> String -> Bool 
noGrays feedback w = all (not . flip elem w') $ grays
    where 
        grays :: [Char]
        grays = map fst $ filter (isGray . snd) feedback
        w' = notGreens feedback w

hasAllYellows :: Feedback -> String -> Bool
hasAllYellows feedback w = all (flip elem w') yellows
    where 
        yellows :: [Char]
        yellows = map fst $ filter (isYellow . snd) feedback
        w' = notGreens feedback w

match :: Feedback -> String -> Bool
match feedback w = 
    matchGreen feedback w
    && matchYellow feedback w
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

main :: IO ()
main = do
    let ws = guessWords ++ finalWords
    putStrLn "instructions for typing colors: "
    putStrLn "type \"gxxxx\" for following color sequence: Green Gray Gray Gray Gray"
    putStrLn "type \"gyxxx\" for following color sequence: Green Yellow Gray Gray Gray"
    putStrLn ""
    step ws []
