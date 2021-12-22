module Adjectives where

type Adjective = String -- dictionary form
type Stem = String
type Truth = Bool

data Tense = NonPerfective | Perfective | Provisional | Gerund | Volitional | PastVolitional | Conditional | Alternative

-- | generate an adjective using the formula
generateAdjective :: Adjective -> Truth -> Tense -> String
generateAdjective adj truth tense = (getStem adj) ++ (intrTruth truth) ++ (intrTense tense)

-- TODO: this part is really messy

-- | get the adjective stem
getStem :: Adjective -> Stem
getStem adj 
    -- na adjective
    | getLastN adj 2 == "na" = take (length adj - 2) adj
    -- i adjective
    | getLastN adj 1 == "i" = take (length adj - 1) adj
    -- invalid
    | otherwise = error "given adjective was neither an i nor a na adjective"

-- | get the last n characters from the right of a string
-- helper function
getLastN :: String -> Int -> String
getLastN str n = drop (length str - n) str

-- interpretation of values

intrTruth :: Truth -> String 
intrTruth False = "kuna"
intrTruth True = ""

intrTense :: Tense -> String
intrTense NonPerfective = "i"
intrTense Perfective = "katta"
intrTense Provisional = "kereba"
intrTense Gerund = "kute"
intrTense Volitional = "sou"
intrTense PastVolitional = "kattarou"
intrTense Conditional = "kattara"
intrTense Alternative = "kattari"


