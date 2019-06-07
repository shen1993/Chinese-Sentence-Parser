module World where

import HRAS
import P
import Data.List
import qualified Data.Map.Strict as M

type Prop = String

data ModalOperator = Necessarily | Possibly deriving (Show, Eq)

data TemporalOperator = H | P | F | G deriving (Show, Eq)

data TProp = TProp { tempOp :: TemporalOperator,
     	     	     prop ::  Prop } deriving (Show, Eq)

data World = World { propositions :: [Prop] } deriving (Show, Eq)

w1 = World { propositions = ["snowwhite will_shout",
                             "snowwhite will_support the man",
                             "alice will_kill snowwhite",
                             "alice will_see a boy",
                             "dorothy will_come",
                             "atreyu finds a girl",
                             "atreyu will_try",
                             "the girls will_hit the boy",
                             "the girl will_become a princess",
                             "a girl will_leave",
                             "the boy hears the girl",
                             "a boy will_appear",
                             "the man believed the woman",
                             "these things will_happen",
                             "the dwarf follows the princess",
                             "some dwarfs will_remember the girl",
                             "few men meet the princess",
                             "all women win"] }
w2 = World { propositions = ["snowwhite shouts",
                            "snowwhite will_support the man",
                            "alice will_kill snowwhite",
                            "alice will_see a boy",
                            "dorothy comes",
                            "atreyu has_found a girl",
                            "atreyu found a girl",
                            "atreyu will_try",
                            "the girls will_hit the boy",
                            "the girl will_become a princess",
                            "a girl leaves",
                            "the boy has_heard the girl",
                            "the boy heard the girl",
                            "a boy appears",
                            "the man believed the woman",
                            "these things will_happen",
                            "the dwarf has_followed the princess",
                            "the dwarf followed the princess",
                            "some dwarfs will_remember the girl",
                            "few men have_met the princess",
                            "few men met the princess",
                            "all women have_won",
                            "all women won"] }
w3 = World { propositions = ["snowwhite has_shouted",
                             "snowwhite shouted",
                             "snowwhite supports the man",
                             "alice will_kill snowwhite",
                             "alice will_see a boy",
                             "dorothy has_come",
                             "dorothy came",
                             "atreyu found a girl",
                             "atreyu tries",
                             "the girls will_hit the boy",
                             "the girl will_become a princess",
                             "a girl has_left",
                             "a girl left",
                             "the boy heard the girl",
                             "a boy has_appeared",
                             "a boy appeared",
                             "the man believed the woman",
                             "these things happen",
                             "the dwarf followed the princess",
                             "some dwarfs remember the girl",
                             "few men met the princess",
                             "all women won"] }
w4 = World { propositions = ["snowwhite shouted",
                             "snowwhite has_supported the man",
                             "snowwhite supported the man",
                             "alice kills snowwhite",
                             "alice will_see a boy",
                             "dorothy came",
                             "atreyu found a girl",
                             "atreyu has_tried",
                             "atreyu tried",
                             "the girls hit the boy",
                             "the girl will_become a princess",
                             "a girl left",
                             "a boy appeared",
                             "the man believed the woman",
                             "the boy heard the girl",
                             "these things have_happened",
                             "these things happened",
                             "the dwarf followed the princess",
                             "some dwarfs have_remembered the girl",
                             "some dwarfs remembered the girl",
                             "few men met the princess",
                             "all women won"] }
w5 = World { propositions = ["snowwhite supported the man",
                             "alice has_killed snowwhite",
                             "alice sees a boy",
                             "dorothy will_want those things",

                             "atreyu found a girl",
                             "atreyu tried",
                             "the girls have_hit the boy",
                             "the grils hit the boy",
                             "the girl becomes a princess",
                             "a girl left",
                             "a boy appeared",
                             "the man believed the woman",
                             "the boy heard the girl",
                             "these things happened",
                             "the dwarf followed the princess",
                             "some dwarfs remembered the girl",
                             "few men met the princess",
                             "all women won"] }

model = [w1,w2,w3,w4,w5]

isValid :: TProp -> Bool

isValid b = if ( tempOp b == F && ( prop b `elem` propositions w5 ) )
            || ( tempOp b == G && ( prop b `elem` propositions w1 
                                 && prop b `elem` propositions w2 
                                 && prop b `elem` propositions w3 
                                 && prop b `elem` propositions w4 
                                 && prop b `elem` propositions w5 ) ) then True else False

isSatisfiable :: TProp -> Bool

isSatisfiable b =  if ( tempOp b == H && ( prop b `elem` propositions w1 ) )
                   || ( tempOp b == P && ( prop b `elem` propositions w1 
                                        || prop b `elem` propositions w2 
                                        || prop b `elem` propositions w3 
                                        || prop b `elem` propositions w4 ) ) 
                   || ( tempOp b == F && ( prop b `elem` propositions w1 
                                        || prop b `elem` propositions w2 
                                        || prop b `elem` propositions w3 
                                        || prop b `elem` propositions w4 
                                        || prop b `elem` propositions w5 ) ) 
                   || ( tempOp b == G && ( prop b `elem` propositions w5 ) ) then True else False


isSatisfied :: TProp -> World -> Bool

isSatisfied b w = if ( tempOp b == F && ( ( w == w1 && ( prop b `elem` propositions w1 
                                                      || prop b `elem` propositions w2 
                                                      || prop b `elem` propositions w3 
                                                      || prop b `elem` propositions w4 
                                                      || prop b `elem` propositions w5 ) )
                                       || ( w == w2 && ( prop b `elem` propositions w2 
                                                      || prop b `elem` propositions w3 
                                                      || prop b `elem` propositions w4 
                                                      || prop b `elem` propositions w5 ) )
                                       || ( w == w3 && ( prop b `elem` propositions w3 
                                                      || prop b `elem` propositions w4 
                                                      || prop b `elem` propositions w5 ) )
                                       || ( w == w4 && ( prop b `elem` propositions w3 
                                                      || prop b `elem` propositions w4 ) )
                                       || ( w == w5 && ( prop b `elem` propositions w5 ) ) ) )
                  || ( tempOp b == P && ( ( w == w2 && ( prop b `elem` propositions w1 ) )
                                       || ( w == w3 && ( prop b `elem` propositions w1 
                                                      || prop b `elem` propositions w2 ) )
                                       || ( w == w4 && ( prop b `elem` propositions w1 
                                                      || prop b `elem` propositions w2 
                                                      || prop b `elem` propositions w3 ) )
                                       || ( w == w5 && ( prop b `elem` propositions w1 
                                                      || prop b `elem` propositions w2 
                                                      || prop b `elem` propositions w3
                                                      || prop b `elem` propositions w4 ) ) ) )
                  || ( tempOp b == G && ( ( w == w1 && ( prop b `elem` propositions w1 
                                                      && prop b `elem` propositions w2 
                                                      && prop b `elem` propositions w3 
                                                      && prop b `elem` propositions w4 
                                                      && prop b `elem` propositions w5 ) )
                                       || ( w == w2 && ( prop b `elem` propositions w2 
                                                      && prop b `elem` propositions w3 
                                                      && prop b `elem` propositions w4 
                                                      && prop b `elem` propositions w5 ) )
                                       || ( w == w3 && ( prop b `elem` propositions w3 
                                                      && prop b `elem` propositions w4 
                                                      && prop b `elem` propositions w5 ) )
                                       || ( w == w4 && ( prop b `elem` propositions w3 
                                                      && prop b `elem` propositions w4 ) )
                                       || ( w == w5 && ( prop b `elem` propositions w5 ) ) ) )
                  || ( tempOp b == H && ( ( w == w2 && ( prop b `elem` propositions w1 ) )
                                       || ( w == w3 && ( prop b `elem` propositions w1 
                                                      && prop b `elem` propositions w2 ) )
                                       || ( w == w4 && ( prop b `elem` propositions w1 
                                                      && prop b `elem` propositions w2 
                                                      && prop b `elem` propositions w3 ) )
                                       || ( w == w5 && ( prop b `elem` propositions w1 
                                                      && prop b `elem` propositions w2 
                                                      && prop b `elem` propositions w3
                                                      && prop b `elem` propositions w4 ) ) ) ) then True
                                                                                               else False



same (x:xs) ys = if x `elem` ys
                 then x:same xs (delete x ys)
                 else same xs ys
same [] _ = []
same _ [] = []

d = M.fromList [("shouted", ("p", "shouts")), ("will_shout",("f", "shouts")), ("has_shouted", ("p", "shouts")),
                 ("supported", ("p", "supports")), ("will_support", ("f", "supports")), ("has_supported", ("p", "supports")),
                 ("killed", ("p", "kills")), ("will_kill", ("f", "kills")), ("has_killed", ("p", "kills")),
                 ("saw", ("p", "sees")), ("will_see", ("f", "sees")), ("has_seen", ("p", "sees")),
                 ("came", ("p", "comes")), ("will_come", ("f", "comes")), ("has_come", ("p", "comes")),
                 ("wanted", ("p", "wants")), ("will_want", ("f", "wants")), ("has_wanted", ("p", "wants")),
                 ("found", ("p", "finds")), ("will_find", ("f", "finds")), ("has_found", ("p", "finds")),
                 ("tried", ("p", "tries")), ("will_try", ("f", "tries")), ("has_tried", ("p", "tries")),
                 ("hit", ("p", "hit")), ("will_hit", ("f", "hit")), ("have_hit", ("p", "hit")),
                 ("became", ("p", "becomes")), ("will_become", ("f", "becomes")), ("has_become", ("p", "becomes")),
                 ("left", ("p", "leaves")), ("will_leave", ("f", "leaves")), ("has_left", ("p", "leaves")),
                 ("appeared", ("p", "appears")), ("will_appear", ("f", "appears")), ("has_appeared", ("p", "appears")),
                 ("heard", ("p", "hears")), ("will_hear", ("f", "hears")), ("has_heard", ("p", "hears")),
                 ("believed", ("p", "believes")), ("will_believe", ("f", "believes")), ("has_believed", ("p", "believes")),
                 ("happened", ("p", "happen")), ("will_happen", ("f", "happen")), ("have_happened", ("p", "happen")),
                 ("followed", ("p", "follow")), ("will_follow", ("f", "follow")), ("has_followed", ("p", "follows")),
                 ("remembered", ("p", "remember")), ("will_remember", ("f", "remember")), ("have_remembered", ("p", "remember")),
                 ("met", ("p", "meet")), ("will_meet", ("f", "meet")), ("have_met",("p",  "meet")),
                 ("won", ("p", "win")), ("will_win", ("f", "win")), ("have_won", ("p", "win"))]
l :: [String]
l = ["shouted","will_shout","has_shouted",
     "supported","will_support","has_supported",
     "killed","will_kill","has_killed", 
     "saw","will_see","has_seen",
     "came","will_come", "has_come", 
     "wanted","will_want","has_wanted", 
     "found", "will_find","has_found", 
     "tried","will_try","has_tried",                
     "hit", "will_hit", "have_hit", 
     "became", "will_become", "has_become",
     "left", "will_leave", "has_left",
     "appeared", "will_appear","has_appeared",
     "heard","will_hear", "has_heard", 
     "believed","will_believe","has_believed",
     "happened","will_happen","have_happened",
     "followed","will_follow","has_followed",
     "remembered", "will_remember",
     "met","will_meet","have_met",
     "won","will_win", "have_won"]


eliminate (Just a) = a
getVerb :: String -> (String, String)
getVerb a = eliminate $ M.lookup a d


entailments :: Prop -> World -> [(ModalOperator, String, Prop)]

parse_ s = P.parses s !! 0
sub_ parse = P.subtrees parse
t2c_ sub = map P.t2c sub
phon_ t2c = map P.phon t2c
tuple_ x = getVerb $ same (phon_ $ t2c_ $ sub_ $ parse_ x) l !! 0
tense_ (x, _) = x
verb_ (_, x) = x
entailments s w = if (w == w1 && (tense_ $ tuple_ s) == "f")
                  then [(Possibly, "w2", verb_ $ tuple_ s),
                        (Possibly, "w3", verb_ $ tuple_ s),
                        (Possibly, "w4", verb_ $ tuple_ s),
                        (Possibly, "w5", verb_ $ tuple_ s)]
                  else if (w == w1 && (tense_ $ tuple_ s) == "p")
                       then []
                  else if (w == w2 && (tense_ $ tuple_ s) == "f")
                       then [(Possibly, "w3", verb_ $ tuple_ s),
                             (Possibly, "w4", verb_ $ tuple_ s),
                             (Possibly, "w5", verb_ $ tuple_ s)]
                  else if (w == w2 && (tense_ $ tuple_ s) == "p")
                       then [(Necessarily, "w1", verb_ $ tuple_ s)]
                  else if (w == w3 && (tense_ $ tuple_ s) == "f")
                       then [(Possibly, "w4", verb_ $ tuple_ s),
                             (Possibly, "w5", verb_ $ tuple_ s)]
                  else if (w == w3 && (tense_ $ tuple_ s) == "p")
                       then [(Possibly, "w1", verb_ $ tuple_ s),
                             (Possibly, "w2", verb_ $ tuple_ s)]
                  else if (w == w4 && (tense_ $ tuple_ s) == "f")
                       then [(Necessarily, "w5", verb_ $ tuple_ s)]
                  else if (w == w4 && (tense_ $ tuple_ s) == "p")
                       then [(Possibly, "w1", verb_ $ tuple_ s),
                             (Possibly, "w2", verb_ $ tuple_ s),
                             (Possibly, "w3", verb_ $ tuple_ s)]
                  else if (w == w5 && (tense_ $ tuple_ s) == "f")
                       then []
                  else if (w == w5 && (tense_ $ tuple_ s) == "p")
                       then [(Possibly, "w1", verb_ $ tuple_ s),
                             (Possibly, "w2", verb_ $ tuple_ s),
                             (Possibly, "w3", verb_ $ tuple_ s),
                             (Possibly, "w4", verb_ $ tuple_ s)]
                  else []


