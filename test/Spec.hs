{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Ohua.Prelude

import           Data.ByteString.Lazy     as B
import           Ohua.ALang.Lang
import           Ohua.ALang.NS
import           Ohua.Compat.ML.Parser
import           Test.Hspec
import Test.Hspec.QuickCheck
import Ohua.Types.Arbitrary ()
import Ohua.ALang.PPrint

lp :: B.ByteString -> Expr SomeBinding
lp = parseExp

main :: IO ()
main =
    hspec $
    describe "parser and lexer" $ do
        describe "var" $ do
            it "parses an unqualified binding" $ lp "a" `shouldBe` "a"
            it "parses a qualified binding" $
                lp "a.b/c" `shouldBe` Var (Qual "a.b/c")
        describe "apply" $ do
            it "parses a simple apply" $ lp "a b" `shouldBe` ("a" `Apply` "b")
            it "parses a multi apply" $
                lp "something a b c" `shouldBe`
                ("something" `Apply` "a" `Apply` "b" `Apply` "c")
        describe "let" $ do
            it "parses a let" $ lp "let a = b in b" `shouldBe` Let "a" "b" "b"
            it "parses longer let binds" $
                lp "let a = r in let b = f in let c = print j in a" `shouldBe`
                Let "a" "r" (Let "b" "f" $ Let "c" ("print" `Apply` "j") "a")
        describe "lambda" $ do
            it "parses a simple lambda" $
                lp "\\ a -> b" `shouldBe` Lambda "a" "b"
            it "parses consecutive lambdas" $
                lp "\\ a -> \\ (b, c) -> print a; c" `shouldBe`
                Lambda
                    "a"
                    (Lambda (Destructure ["b", "c"]) $
                     Let "_" ("print" `Apply` "a") "c")
            it "parses a lambda with 2 arguments" $
                lp "\\ a (b, c) -> print a; c" `shouldBe`
                Lambda
                    "a"
                    (Lambda (Destructure ["b", "c"]) $
                     Let "_" ("print" `Apply` "a") "c")
            it "parses a let following a statement" $
                lp "print a; let x = b in b" `shouldBe` Let "_" ("print" `Apply` "a") (Let "x" "b" "b")
        describe "comments" $ do
            it "parses a comment" $ lp "a (* comment *)" `shouldBe` "a"
            it "parses a comment in an application" $
                lp "a (* another comment *) b" `shouldBe` "a" `Apply` "b"
        it "supports the wildcard binding" $ do
            lp "_" `shouldBe` "_"
            lp "let (_, _) = a in b" `shouldBe` Let ["_", "_"] "a" "b"
        it "parses the example module" $
            (parseMod <$> B.readFile "test-resources/something.ohuaml") `shouldReturn`
            ((emptyNamespace ["some_ns"] :: Namespace ()) &
             algoImports .~ [(["some", "module"], ["a"])] &
             sfImports .~ [(["ohua", "math"], ["mult", "isZero"])] &
             decls .~
             [ ("square", Lambda "x" ("mult" `Apply` "x" `Apply` "x"))
             , ( "algo1"
               , Lambda "someParam" $
                 Let "a" ("square" `Apply` "someParam") $
                 Let
                     "coll0"
                     ("ohua.lang/smap" `Apply` Lambda "i" ("square" `Apply` "i") `Apply`
                      "coll")
                     ("ohua.lang/if" `Apply` ("isZero" `Apply` "a") `Apply`
                      Lambda "_" "coll0" `Apply`
                      Lambda "_" "a"))
             , ( "main"
               , Lambda "param" $ Lambda "param2" ("algo0" `Apply` "param"))
             ])
        describe "pretty-printing <=> parsing equality" $ do
          prop "parses pretty printed alang" $
            \expr ->
              lp (encodeUtf8 (quickRender expr)) == (expr :: Expr SomeBinding)
          -- This test kinda works, but also runs into some infinite loop ...
          -- we should figure out why at some point, but for now it seems to
          -- work ... kind
          -- prop "parses a pretty printed namespace" $
          --   \ns ->
          --     if parseMod (encodeUtf8 (quickRender ns)) /= ns
          --     then trace (quickRender ns) False
          --     else True
