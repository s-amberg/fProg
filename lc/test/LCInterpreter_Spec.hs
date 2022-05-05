module LCInterpreter_Spec where

import Test.Hspec
import Test.QuickCheck
import Test.Validity
import LCInterpreter ( Term(Var, Abs, App), freeVars, substitute, incrementVar, reduce, derivation, leftmostOutermost, leftmostInnermost )

-- primitive equality for tests
instance Eq Term where
    (==) (Var a) (Var b) = a == b
    (==) (Abs a as) (Abs b bs) = a == b && as == bs
    (==) (App ta1 ta2) (App tb1 tb2) = ta1 == tb1 && ta2 == tb2
    (==) _ _ = False

spec :: Spec
spec = do

    

    describe "LCInterpreter" $ do
        let id = App (Abs "x" (Var "x")) (Var "a")
        -- t2 = ((λx.λy.x y) y) ((λx.λy.x y) y c)
        -- the y both are free and different variables, c free
        let t2 = App (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y")) (App (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y")) (Var "c"))
        let t3 = App (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y2")) (App (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y3")) (Var "c"))

        describe "incrementVar" $ do
            it "increments a new variable to 1" $
                incrementVar "y" `shouldBe` "y1"
            it "increments an incremented variable by 1" $
                incrementVar "y31" `shouldBe` "y32"

        describe "freeVars" $ do
            it "returns a set of all free variables in a term id" $
                freeVars id `shouldBe` ["a"]
            it "returns a set of all free variables in a term App with multiple free Vars with same name" $
                freeVars t2 `shouldBe` ["c", "y"]
            it "returns a set of all free variables in a term App with var in 1 and 2" $
                freeVars t3 `shouldBe` ["y3", "c", "y2"]

        describe "substitue" $ do
            it "substitutes x with L in y if x and y are distinct" $
                substitute "x" id (Var "y") `shouldBe` (Var "y")
            it "substitutes x with L in y if x and y are not distinct" $
                substitute "x" id (Var "x") `shouldBe` id
            it "substitutes x with L in \\x.M" $
                substitute "x" id (Abs "x" t2) `shouldBe` (Abs "x" t2)
            -- λy.(x(λx.xy))
            let expr = Abs "y" (App (Var "x") (Abs "x" (App (Var "x") (Var "y"))))
            -- λy.(λy(λy.x(λx.xy)))
            let expr2 = Abs "y" (Abs "y" (Abs "y"(App (Var "x") (Abs "x" (App (Var "x") (Var "y"))))))
            -- λy. y (λy(λy.x(λx.xy)))
            let expr3 = Abs "y" (App (Var "y") (Abs "y" (Abs "y"(App (Var "x") (Abs "x" (App (Var "x") (Var "y")))))))
            it "substitutes x with L in \\x.M if x,y are distinct and y nfin L" $
                substitute "x" (Var "z") expr `shouldBe` Abs "y" (App (Var "z") (Abs "x" (App (Var "x") (Var "y"))))
            it "substitutes x with L in \\y.M if x,y are distinct but NOT y nfin L" $
                substitute "x" (Var "y") expr `shouldBe` Abs "y1" (App (Var "y") (Abs "x" (App (Var "x") (Var "y1"))))
            it "substitutes x with L in \\y.M if x,y are distinct but NOT y nfin L with nested Abs" $
                substitute "x" (Var "y") expr2 `shouldBe` Abs "y1" (Abs "y2" (Abs "y3"(App (Var "y") (Abs "x" (App (Var "x") (Var "y3"))))))
            it "substitutes x with L in \\y.M if x,y are distinct but NOT y nfin L with App of nested Abs" $
                substitute "x" (Var "y") expr3 `shouldBe` Abs "y1" (App (Var "y1") (Abs "y2" (Abs "y3"(App (Var "y") (Abs "x" (App (Var "x") (Var "y3")))))))

        describe "reduce" $ do
            it "`(λx.x) a` reduces to `a`" $
                reduce (App (Abs "x" (Var "x")) (Var "a")) `shouldBe` (Var "a")
            it "`(λx.x x) (λx.x)` reduces to `λx.x`" $
                reduce (App (Abs "x" (App (Var "x") (Var "x"))) (Abs "x" (Var "x"))) `shouldBe` (Abs "x" (Var "x")) 
            it "`(λx.λy.x y) y` reduces to `λy1.y y1`, and not `λy.y y`" $
                reduce (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y")) `shouldBe` (Abs "y1" (App (Var "y") (Var "y1")))
            it "`(λx.λy.x y) y) ((λx.λy.x y) y c` reduces to y(yc)" $
                reduce t2 `shouldBe` (App (Var "y") (App (Var "y") (Var "c")))

        describe "derivation" $ do
            let steps = [App (Var "y") (App (Var "y") (Var "c")), App (Var "y") (App (Abs "y1" (App (Var "y") (Var "y1"))) (Var "c")), App (Var "y") (App (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y")) (Var "c")), App (Abs "y1" (App (Var "y") (Var "y1"))) (App (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y")) (Var "c")), t2]
            it "should return a list of all reductionsteps" $
               derivation leftmostOutermost t2 `shouldBe` steps