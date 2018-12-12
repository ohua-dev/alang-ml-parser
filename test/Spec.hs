{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Ohua.Prelude

import           Data.ByteString.Lazy     as B
import           Test.Hspec
import Test.Hspec.QuickCheck

import Ohua.Frontend.Lang
import Ohua.Frontend.NS
import Ohua.Compat.ML.Parser
-- import Ohua.Types.Arbitrary ()
import Ohua.Unit

lp :: B.ByteString -> Expr
lp = parseExp

pattern Apply a b <- AppE a [b]
    where Apply a b = AppE a [b]
pattern SfE a b = LitE (FunRefLit (FunRef a b))

main :: IO ()
main =
    hspec $
    describe "parser and lexer" $ do
        describe "var" $ do
            it "parses an unqualified binding" $ lp "a" `shouldBe` "a"
            it "supports the wildcard binding" $ do
                lp "_" `shouldBe` "_"
                lp "let (_, _) = a in b" `shouldBe` LetE ["_", "_"] "a" "b"
        describe "literals" $ do
            it "parses env refs" $ lp "$1" `shouldBe` LitE (EnvRefLit 1)
            it "parses unit" $ lp "()" `shouldBe` LitE UnitLit
            it "parses a qualified binding" $
                lp "a.b/c" `shouldBe` SfE "a.b/c" Nothing
            it "parses integer literals" $ do
                lp "0" `shouldBe` LitE (NumericLit 0)
                lp "1" `shouldBe` LitE (NumericLit 1)
                lp "1000030" `shouldBe` LitE (NumericLit 1000030)
            it "parses negative literals" $ do
                lp "-1" `shouldBe` LitE (NumericLit (-1))
                lp "- 1" `shouldBe` LitE (NumericLit (-1))
                lp "-1000030" `shouldBe` LitE (NumericLit (-1000030))
            it "() is a valid pattern" $
                lp "\\() -> a" `shouldBe` LamE [UnitP] "a"
        describe "apply" $ do
            it "parses a simple apply" $ lp "a b" `shouldBe` AppE "a" ["b"]
            it "parses a multi apply" $
                lp "something a b c" `shouldBe`
                ("something" `Apply` "a" `Apply` "b" `Apply` "c")
        describe "let" $ do
            it "parses a let" $ lp "let a = b in b" `shouldBe` LetE "a" "b" "b"
            it "parses longer let binds" $
                lp "let a = r in let b = f in let c = print j in a" `shouldBe`
                LetE "a" "r" (LetE "b" "f" $ LetE "c" ("print" `Apply` "j") "a")
        describe "lambda" $ do
            it "parses a simple lambda" $
                lp "\\ a -> b" `shouldBe` LamE ["a"] "b"
            it "parses consecutive lambdas" $
                lp "\\ a -> \\ (b, c) -> print a; c" `shouldBe`
                LamE ["a"] (LamE [["b", "c"]] $ StmtE ("print" `Apply` "a") "c")
            it "parses a lambda with 2 arguments" $
                lp "\\ a (b, c) -> print a; c" `shouldBe`
                LamE ["a", ["b", "c"]] (StmtE ("print" `Apply` "a") "c")
            it "parses a let following a statement" $
                lp "print a; let x = b in b" `shouldBe`
                StmtE ("print" `Apply` "a") (LetE "x" "b" "b")
        describe "state binding" $ do
            it "parses a simple state binding" $
                lp "x with a" `shouldBe` BindE "x" "a"
            it ".. with literal" $ do
                lp "x with ()" `shouldBe` BindE "x" (LitE UnitLit)
                lp "x with 1" `shouldBe` BindE "x" (LitE (NumericLit 1))
            describe "precedence" $ do
                it "apply before bind" $
                    lp "x with a b" `shouldBe` BindE "x" (AppE "a" ["b"])
                it "parenthesized bind" $
                    lp "(x with a) b" `shouldBe` AppE (BindE "x" "a") ["b"]
                it "let before bind" $
                    lp "x with let y = a in y" `shouldBe` BindE "x" (LetE "y" "a" "y")
                it "bind in let" $
                    lp "let y = x with a in y" `shouldBe` LetE "y" (BindE "x" "a") "y"
        describe "comments" $ do
            it "parses a comment" $ lp "a (* comment *)" `shouldBe` "a"
            it "parses a comment in an application" $
                lp "a (* another comment *) b" `shouldBe` "a" `Apply` "b"
        it "parses the example module" $
            (parseMod <$> B.readFile "test-resources/something.ohuaml") `shouldReturn`
            ((emptyNamespace ["some_ns"] :: Namespace ()) &
             algoImports .~ [(["some", "module"], ["a"])] &
             sfImports .~ [(["ohua", "math"], ["mult", "isZero"])] &
             decls .~
             [ ("square", LamE ["x"] ("mult" `Apply` "x" `Apply` "x"))
             , ( "algo1"
               , LamE ["someParam"] $
                 LetE "a" ("square" `Apply` "someParam") $
                 LetE
                     "coll0"
                     (SfE "ohua.lang/smap" Nothing `Apply`
                      LamE ["i"] ("square" `Apply` "i") `Apply`
                      "coll")
                     (IfE ("isZero" `Apply` "a") "coll0" "a"))
             , ("main", LamE ["param", "param2"] ("algo0" `Apply` "param"))
             ])
        -- describe "pretty-printing <=> parsing equality" $ do
        --   prop "parses pretty printed alang" $
        --     \expr ->
        --       lp (encodeUtf8 (quickRender expr)) == (expr :: Expr SomeBinding)
          -- This test kinda works, but also runs into some infinite loop ...
          -- we should figure out why at some point, but for now it seems to
          -- work ... kind
          -- prop "parses a pretty printed namespace" $
          --   \ns ->
          --     if parseMod (encodeUtf8 (quickRender ns)) /= ns
          --     then trace (quickRender ns) False
          --     else True
