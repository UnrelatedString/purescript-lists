module Test.Type.ClassLaws.TestData
  ( arrays
  , functions1
  ) where

import Data.List ((:), List(Nil))
import Test.Type.ClassLaws (HeavenlyStem(..), LawTestChoice, exhaustive)

-- Psseudorandomly generated then hardcoded because the alternative is reimplementing an LCG

arrays :: LawTestChoice (Array HeavenlyStem)
arrays =
  -- handwritten edge cases
    []
  : [Yi]
  : [Wu, Wu]
  : [Ding, Ding, Ding, Ding, Ding, Ding, Ding, Ding, Ding, Ding, Ding, Ding, Ding, Ding, Ding]
  : [Ren, Ji, Ren]
  : [Gui, Jia]
  -- random
  : [Ren, Wu, Ren, Ji, Ding, Ji, Ren, Geng, Jia, Ding, Ding, Bing, Yi]
  : [Ren, Xin, Yi, Wu, Yi, Xin, Geng]
  : [Geng, Ding, Ding, Ren, Ding, Ding, Ji, Yi, Ren, Ji, Wu, Ren, Bing, Wu, Ren, Wu, Bing]
  : [Wu, Ji, Xin, Ding, Gui, Ding, Yi, Xin, Gui]
  : [Geng, Geng, Bing, Gui, Geng, Gui, Bing, Ji, Bing, Wu, Bing, Ji, Wu, Yi, Ren]
  : [Xin, Ding, Gui, Jia, Yi]
  : [Ding, Jia, Xin, Wu, Gui, Xin, Xin, Xin, Ren, Geng, Gui, Geng, Geng, Ding, Ji]
  : [Wu, Ren, Jia, Ding, Ji, Geng, Wu, Jia, Gui, Jia, Gui, Xin, Wu, Bing, Geng, Yi, Ji, Ding, Wu, Ding, Ji, Jia]
  : [Bing, Jia, Jia, Bing, Geng, Gui, Bing, Ren, Jia, Wu, Ding, Geng, Wu, Jia]
  : [Jia, Ji, Bing, Ren, Ren, Ding, Gui, Gui]
  : [Gui, Ding, Gui, Ding]
  : [Ji, Geng, Jia, Ren]
  : [Ji, Xin, Wu, Gui, Jia, Xin, Wu, Yi, Ding, Geng, Ji, Xin, Bing, Wu, Yi]
  : [Xin, Xin, Yi, Ding, Jia, Geng, Gui, Yi, Bing, Geng, Ren]
  : [Ding, Gui, Yi, Geng, Yi, Geng, Ji, Ding, Xin, Ding, Jia, Bing, Wu, Ding]
  : [Ren, Jia, Jia, Jia, Wu, Yi, Ji]
  : [Ren, Ding, Ji, Wu, Ding, Ren, Ji, Bing, Geng, Ren, Xin, Ji, Bing]
  : [Ding, Bing, Xin, Ren, Ji, Xin, Ren, Geng, Ji, Ding, Bing, Wu]
  : [Xin, Wu, Bing, Ren]
  : [Ding, Wu, Xin, Wu, Ji, Yi, Xin, Ren, Xin, Yi, Wu, Ding, Bing, Ji, Xin, Ding, Ren, Xin, Gui, Gui, Wu]
  : [Bing, Ren, Ji, Gui, Bing, Gui, Geng, Jia, Bing, Jia, Jia, Ding, Xin, Jia, Bing]
  : [Ren, Yi, Gui, Xin, Xin, Gui, Wu, Ji, Ding, Jia, Gui, Yi, Yi, Wu, Ren, Gui, Bing, Jia, Ji, Ji, Bing, Geng]
  : [Jia, Ding, Wu, Bing, Bing, Yi, Ding, Xin]
  : [Ji, Ding, Jia, Jia, Gui, Gui, Wu, Bing, Jia, Ji]
  : [Ji, Ren, Ji, Ren, Yi, Geng, Ding, Gui]
  : [Ding, Ren, Gui, Jia, Wu, Ji, Ding]
  : [Yi, Wu, Ren, Geng, Geng, Wu, Ding]
  : [Yi, Gui, Wu, Yi, Ren, Ren, Ji, Ji, Yi, Gui, Xin, Wu, Wu, Gui, Ren, Xin, Geng, Yi, Xin, Gui, Xin, Jia, Wu]
  : [Yi, Bing, Ji, Geng, Geng, Wu, Bing, Xin, Gui]
  : [Ji, Xin, Gui, Bing, Ding, Ding, Gui, Geng, Gui, Wu, Xin, Geng, Geng, Wu, Geng]
  : [Yi, Ji, Gui, Yi]
  : [Xin, Geng, Jia, Geng, Wu, Ding, Jia]
  : [Yi, Ren, Ding]
  : [Bing, Ding, Jia, Ren, Xin, Bing, Gui, Ding, Xin, Ji, Xin, Geng, Ding, Ding, Yi, Jia, Geng]
  : [Geng, Jia, Ding, Geng, Jia, Yi, Ren, Wu, Bing, Geng, Geng, Ding, Xin, Jia, Gui, Jia, Ji, Ding, Xin, Geng]
  : [Geng, Jia, Ding, Ding, Jia, Yi, Ji, Xin, Ji, Gui, Bing, Wu, Gui, Gui]
  : [Ren, Xin, Yi, Ding, Bing, Ding, Geng, Ding, Geng, Wu, Ding, Ren, Ren, Wu, Gui, Geng, Jia, Ding, Ji, Geng]
  : [Ji, Geng, Ren, Ding, Ding, Yi, Gui, Yi, Ding]
  : [Geng, Ji, Ji, Geng, Ding, Jia, Gui, Yi, Ding, Geng]
  : [Jia, Yi, Gui, Yi, Yi, Geng, Ji, Jia, Geng, Geng, Gui, Ding, Ren, Geng, Wu, Jia, Ding, Ren, Geng, Gui]
  : Nil

functions1 :: LawTestChoice (HeavenlyStem -> HeavenlyStem)
functions1 =
  -- handwritten
    identity
  : const Wu
  : const Geng
  : exhaustive Yi Bing Ding Wu Ji Geng Xin Ren Gui Jia
  -- random
  : exhaustive Ren Bing Yi Xin Bing Bing Ji Bing Geng Xin
  : exhaustive Geng Yi Wu Gui Wu Yi Ren Xin Yi Ding
  : exhaustive Geng Bing Gui Jia Ji Ding Yi Gui Geng Jia
  : exhaustive Ren Xin Jia Wu Ren Xin Geng Ding Xin Ren
  : exhaustive Gui Ding Ding Xin Wu Gui Jia Ji Wu Geng
  : exhaustive Wu Gui Ren Ding Geng Ji Yi Jia Yi Yi
  : exhaustive Gui Ji Geng Bing Geng Bing Bing Ren Gui Ren
  : exhaustive Ding Geng Geng Ren Ji Geng Jia Xin Jia Ding
  : exhaustive Wu Jia Ding Wu Yi Ren Bing Gui Bing Xin
  : exhaustive Yi Ren Ji Xin Ji Ren Ding Ding Geng Ding
  : exhaustive Bing Jia Bing Bing Ji Jia Ren Xin Ji Jia
  : exhaustive Ji Geng Yi Ji Ding Gui Ding Ren Xin Bing
  : exhaustive Geng Ren Bing Yi Ji Ren Geng Jia Yi Ji
  : exhaustive Ji Gui Jia Ding Geng Ji Ding Xin Ding Jia
  : exhaustive Ji Yi Bing Ding Ding Ding Ren Wu Gui Gui
  : exhaustive Yi Bing Gui Ding Ji Bing Gui Ding Jia Jia
  : exhaustive Jia Bing Jia Bing Bing Gui Ding Geng Jia Ren
  : exhaustive Yi Ding Ding Gui Yi Xin Yi Jia Yi Bing
  : exhaustive Bing Gui Gui Bing Wu Yi Xin Ren Geng Bing
  : exhaustive Geng Yi Ji Gui Jia Bing Wu Gui Xin Xin
  : Nil

functions2 :: LawTestChoice (HeavenlyStem -> HeavenlyStem -> HeavenlyStem)
functions2 =
  -- hand
    const
  : const identity
  : const (const Xin)
  : \a b -> if a == b then Bing else Gui
  -- rand
  -- but only two because like wow . that's 100
  : exhaustive
      (exhaustive Geng Gui Geng Xin Wu Gui Geng Wu Geng Jia)
      (exhaustive Ji Geng Ji Yi Ren Ding Xin Ren Ren Geng)
      (exhaustive Bing Jia Wu Ren Wu Jia Ding Bing Gui Xin)
      (exhaustive Gui Jia Gui Ding Ji Bing Yi Geng Ji Geng)
      (exhaustive Gui Wu Gui Geng Xin Ding Ren Wu Ji Xin)
      (exhaustive Yi Ding Jia Gui Ren Ren Ren Ding Wu Gui)
      (exhaustive Ji Ding Ding Ren Gui Geng Xin Jia Yi Geng)
      (exhaustive Ji Ji Xin Ren Ding Jia Xin Wu Ji Ren)
      (exhaustive Wu Wu Yi Yi Ji Gui Bing Bing Gui Yi)
      (exhaustive Wu Jia Ren Jia Geng Gui Wu Xin Jia Xin)
      (exhaustive Yi Ren Ding Ji Wu Bing Yi Xin Ren Ji)
      (exhaustive Ding Ji Yi Xin Jia Bing Bing Xin Ji Ji)
      (exhaustive Xin Wu Xin Xin Ren Wu Bing Yi Ji Jia)
      (exhaustive Gui Yi Ren Xin Ji Wu Geng Gui Wu Jia)
      (exhaustive Bing Xin Ren Jia Ding Ren Yi Wu Ren Geng)
      (exhaustive Jia Ding Bing Ji Xin Xin Wu Bing Ren Xin)
      (exhaustive Ji Ding Ji Wu Wu Yi Ding Ji Ding Ren)
      (exhaustive Xin Gui Yi Geng Xin Ren Gui Ren Geng Gui)
      (exhaustive Geng Yi Ji Bing Wu Wu Wu Gui Yi Yi)
      (exhaustive Xin Bing Ding Bing Wu Gui Ding Ren Gui Ding)
      (exhaustive Ding Yi Ji Geng Ding Ren Ji Bing Yi Xin)
  : exhaustive
      (exhaustive Ren Wu Bing Geng Yi Bing Gui Jia Yi Geng)
      (exhaustive Ding Xin Yi Gui Xin Ren Yi Bing Gui Yi)
      (exhaustive Ren Jia Ren Jia Ren Ding Ji Xin Ren Yi)
      (exhaustive Xin Ren Yi Ji Bing Geng Bing Geng Jia Ding)
      (exhaustive Gui Wu Ji Xin Xin Jia Xin Ding Jia Gui)
      (exhaustive Wu Ji Jia Ren Yi Ding Ren Gui Geng Yi)
      (exhaustive Ji Ren Ren Ren Bing Ding Yi Jia Xin Yi)
      (exhaustive Ji Bing Geng Ding Ren Gui Wu Xin Xin Xin)
      (exhaustive Bing Bing Ren Yi Yi Ren Bing Jia Geng Ji)
      (exhaustive Geng Bing Gui Ding Xin Jia Yi Geng Wu Geng)
      (exhaustive Yi Geng Yi Gui Geng Xin Ding Jia Gui Ren)
      (exhaustive Bing Xin Wu Bing Ren Wu Wu Geng Wu Yi)
      (exhaustive Geng Wu Bing Wu Bing Geng Xin Xin Geng Wu)
      (exhaustive Geng Gui Ding Wu Yi Wu Bing Ding Ji Ji)
      (exhaustive Yi Bing Geng Gui Xin Xin Gui Yi Yi Ji)
      (exhaustive Ding Jia Gui Ren Wu Gui Wu Ji Gui Wu)
      (exhaustive Ren Ding Ji Geng Jia Ding Gui Bing Gui Geng)
      (exhaustive Wu Geng Xin Wu Xin Gui Ding Ren Wu Jia)
      (exhaustive Ji Ji Ren Gui Jia Wu Xin Bing Ren Ding)
