module Test.Chapter3
    ( chapter3
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Chapter3


chapter3 :: Spec
chapter3 = describe "Chapter3" $ do
    describe "Chapter3Normal" $ it "" $ True `shouldBe` True
    describe "Chapter3Advanced" $ it "" $ True `shouldBe` True
    describe "Task2" $ do
        it "knight wins" $ do
            let knight = MkKnight
                        { knightHealth = 10
                        , knightAttack = 2
                        , knightGold = 4
                        }
                monster = MkMonster
                        { monsterHealth = 10
                        , monsterAttack = 1
                        , monsterGold = 4
                        }
                in fight knight monster `shouldBe` 8
        it "monster wins" $ do
            let knight = MkKnight
                        { knightHealth = 10
                        , knightAttack = 2
                        , knightGold = 4
                        }
                monster = MkMonster
                        { monsterHealth = 10
                        , monsterAttack = 4
                        , monsterGold = 4
                        }
                in fight knight monster `shouldBe` -1
    describe "Task8: isWeekend" $ do
        it "is weekend" $ isWeekend Sunday `shouldBe` True
        it "not weekend" $ isWeekend Monday `shouldBe` False
    describe "Task8: nextDay" $ do
        it "Sunday -> Monday" $ nextDay Sunday `shouldBe` Monday
        it "Saturday -> Sunday" $ nextDay Saturday `shouldBe` Sunday
    describe "Task8: daysToParty" $ do
        it "Sunday" $ daysToParty Sunday `shouldBe` 5
        it "Saturday" $ daysToParty Saturday `shouldBe` 6
    describe "Task9" $ do
        it "knight => monster" $ do
            let kknight = MkKKnight
                        { kknightHealth = 10
                        , kknightAttack = 2
                        , kknightDefence = 3
                        }
                mmonster = MkMMonster
                        { mmonsterHealth = 10
                        , mmonsterAttack = 4
                        }
                in battle kknight mmonster `shouldBe` FighterAWin
        it "monster => knight" $ do
            let kknight = MkKKnight
                        { kknightHealth = 10
                        , kknightAttack = 2
                        , kknightDefence = 3
                        }
                mmonster = MkMMonster
                        { mmonsterHealth = 10
                        , mmonsterAttack = 6
                        }
                in battle kknight mmonster `shouldBe` FighterBWin
        it "knight => knight" $ do
            let kknightA = MkKKnight
                        { kknightHealth = 10
                        , kknightAttack = 5
                        , kknightDefence = 3
                        }
                kknightB = MkKKnight
                        { kknightHealth = 10
                        , kknightAttack = 4
                        , kknightDefence = 3
                        }
                in battle kknightA kknightB `shouldBe` FighterAWin
        it "monster => monster" $ do
            let mmonsterA = MkMMonster
                        { mmonsterHealth = 10
                        , mmonsterAttack = 4
                        }
                mmonsterB = MkMMonster
                        { mmonsterHealth = 10
                        , mmonsterAttack = 5
                        }
                in battle mmonsterA mmonsterB `shouldBe` FighterBWin
