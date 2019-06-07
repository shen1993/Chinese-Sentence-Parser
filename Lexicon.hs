module Lexicon where

type CatLabel = String
type Phon     = String

data Cat      = Cat Phon CatLabel Agreement [Cat]
              deriving Eq

type Agreement = [Feat]

data Feat = Masc  | Fem  | Neutr | MascOrFem
          | Sg    | Pl
          | Fst   | Snd  | Thrd
          | Nom   | AccOrDat
          | Pers  | Refl | Wh
          | MainVP
          | Past  | Pres | Fut   | Perf | Infl
          | On    | With | By    | To   | From
          deriving (Eq,Show,Ord)

lexicon :: String -> [Cat]

lexicon "我"   = [Cat "我" "NP" []   []]
lexicon "你"   = [Cat "你" "NP" []   []]
lexicon "他"   = [Cat "他" "NP" []   []]
lexicon "她"   = [Cat "她" "NP" []   []]
lexicon "我们"   = [Cat "我们" "NP" []   []]
lexicon "他们"   = [Cat "他们" "NP" []   []]
lexicon "她们"   = [Cat "她们" "NP" []   []]

lexicon "张三" = [Cat "张三" "NP" []   []]
lexicon "李四" = [Cat "李四" "NP" []   []]
lexicon "小明" = [Cat "小明" "NP" []   []]
lexicon "小红" = [Cat "小红" "NP" []   []]
lexicon "小刚" = [Cat "小刚" "NP" []   []]
lexicon "小王" = [Cat "小王" "NP" []   []]
lexicon "小李" = [Cat "小李" "NP" []   []]
lexicon "李白" = [Cat "李白" "NP" []   []]
lexicon "杜甫" = [Cat "杜甫" "NP" []   []]

lexicon "桌子"   = [Cat "桌子" "NP" []   [],
                  Cat "桌子" "CN" []   []]
lexicon "椅子"   = [Cat "椅子" "NP" []   [],
                  Cat "椅子" "CN" []   []]
lexicon "伞"   = [Cat "伞" "NP" []   [],
                  Cat "伞" "CN" []   []]
lexicon "书"   = [Cat "书" "NP" []   [],
                  Cat "书" "CN" []   []]
lexicon "诗"   = [Cat "诗" "NP" []   [],
                  Cat "诗" "CN" []   []]
lexicon "饭"   = [Cat "饭" "NP" []   [],
                  Cat "饭" "CN" []   []]
lexicon "酒"   = [Cat "酒" "NP" []   [],
                  Cat "酒" "CN" []   []]
lexicon "作业"   = [Cat "作业" "NP" []   [],
                    Cat "作业" "CN" []   []]
lexicon "飞机"   = [Cat "飞机" "NP" []   [],
                  Cat "飞机" "CN" []   []]
lexicon "北京"   = [Cat "北京" "NP" []   [],
                  Cat "北京" "CN" []   []]
lexicon "美国"   = [Cat "美国" "NP" []   [],
                    Cat "美国" "CN" []   []]

lexicon "一"   = [Cat "一" "NUM" []  []]
lexicon "两"   = [Cat "两" "NUM" []  []]
lexicon "三"   = [Cat "三" "NUM" []  []]
lexicon "那"   = [Cat "那" "NUM" []  []]
lexicon "这"   = [Cat "这" "NUM" []  []]

lexicon "本"   = [Cat "本" "CLA" []  []]
lexicon "个"   = [Cat "个" "CLA" []  []]
lexicon "张"   = [Cat "张" "CLA" []  []]
lexicon "把"   = [Cat "把" "CLA" []  []]
lexicon "首"   = [Cat "把" "CLA" []  []]

lexicon "了"   = [Cat "了" "PAR" [Past]  []]
lexicon "将要"   = [Cat "将要" "PAR" [Fut]  []]

lexicon "有"   = [Cat "有" "VP" []  [Cat "_" "NP" [AccOrDat] []]]
-- lexicon "有"   =
--   [Cat "有" "VP" [Past]  [Cat "_" "NP" [AccOrDat] [],
--                           Cat "了" "PAR" [Past]  []]]
-- lexicon "有"   =
--   [Cat "有" "VP" [Past]  [Cat "了" "PAR" [Past]  [],
--                           Cat "_" "NP" [AccOrDat] []]]
lexicon "看"   = [Cat "看" "VP" []  [Cat "_" "NP" [AccOrDat] []]]
lexicon "看见"   = [Cat "看见" "VP" []  [Cat "_" "NP" [AccOrDat] []]]
lexicon "写"   = [Cat "写" "VP" []  [Cat "_" "NP" [AccOrDat] []]]
lexicon "读"   = [Cat "读" "VP" []  [Cat "_" "NP" [AccOrDat] []]]
lexicon "坐"   = [Cat "坐" "VP" []  [Cat "_" "NP" [AccOrDat] []]]
lexicon "去"   = [Cat "去" "VP" []  [Cat "_" "NP" [AccOrDat] []]]
lexicon "吃"   = [Cat "吃" "VP" []  [Cat "_" "NP" [AccOrDat] []]]
lexicon "喝"   = [Cat "喝" "VP" []  [Cat "_" "NP" [AccOrDat] []]]
lexicon "认识"   = [Cat "认识" "VP" []  [Cat "_" "NP" [AccOrDat] []]]
lexicon "知道"   = [Cat "知道" "VP" []  [Cat "_" "NP" [AccOrDat] []]]


lexicon "出海"   = [Cat "出海" "VP" []  []]
lexicon "捕鱼"   = [Cat "捕鱼" "VP" []  []]
-- lexicon "出海"   = [Cat "出海" "VP" [Past]  [],
--                    Cat "了" "PAR" [Past]  []]
-- lexicon "出海"   = [Cat "将要" "PAR" [Fut]  [],
--                    Cat "出海" "VP" [Fut]  []]
-- lexicon "捕鱼"   = [Cat "将要" "PAR" [Fut]  [],
--                    Cat "捕鱼" "VP" [Fut]  []]
-- lexicon "捕鱼"   = [Cat "捕鱼" "VP" [Past]  [],
--                    Cat "了" "PAR" [Past]  []]
lexicon "学习"   = [Cat "学习" "VP" []  []]
lexicon "回来"   = [Cat "回来" "VP" []  []]
lexicon "休息"   = [Cat "休息" "VP" []  []]
lexicon "睡"   = [Cat "睡" "VP" []  []]
lexicon "赢"   = [Cat "赢" "VP" []  []]
lexicon "输"   = [Cat "输" "VP" []  []]



--lexicon "将要"    = [Cat "将要"    "TENSE" [Fut] []]

lexicon _ = []
