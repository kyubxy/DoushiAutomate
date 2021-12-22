-- just temporary
module Verbs where

data Honorific = Formal | Plain | None deriving Show

type Verb = String

-- function is shit because it doesn't recognise past tense
getVerbHonorific :: Verb -> Honorific
getVerbHonorific word  
    -- plain form
    | drop (length word - 1) word == "u" = Plain
    -- Formal
    | drop (length word - 4) word == "masu" = Formal
    -- Invalid
    | otherwise = None
    
data VerbBase = Ichidan | Godan | Henkaku  deriving Show

-- | determines whether a verb is ichidan, godan or is irregular
--   will throw an error for words which are not verbs
getVerbBase :: Verb -> VerbBase
getVerbBase verb 
    -- not a verb
    | drop (length verb - 1) verb /= "u" = error "not a verb"
    -- irregular (suru/kuru)
    | verb `elem` henkakuDoushi = Henkaku
    -- ends in ru
    | drop (length verb - 2) verb == "ru" = 
        if ichidanRu then Ichidan else Godan
    -- ends in some other -u, must be godan
    | otherwise = Godan
    where
        beforeru  = verb !! (length verb - 3)
        ichidanRuChara = ['i', 'e']
        ichidanRu = beforeru `elem` ichidanRuChara
        henkakuDoushi = ["suru", "kuru"]

