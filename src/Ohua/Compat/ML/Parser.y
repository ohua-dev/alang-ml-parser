{
-- |
-- Module      : $Header$
-- Description : Parser for ALang ML-Expressions
-- Copyright   : (c) Justus Adam 2018. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : dev@justus.science
-- Stability   : experimental

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE OverloadedStrings, LambdaCase, TupleSections, TypeFamilies, FlexibleContexts #-}
module Ohua.Compat.ML.Parser
    ( parseMod, parseExp
    , Namespace(..)
    ) where

import Ohua.Prelude

import Ohua.Compat.ML.Lexer
import Ohua.ALang.Lang
import Ohua.ALang.NS
import qualified Data.HashMap.Strict as HM
import qualified Ohua.ParseTools.Refs as Refs
import qualified Ohua.ALang.Refs as Refs

import Prelude ((!!))

}


%name parseExpRaw Exp
%name parseModRaw Module
%tokentype { Lexeme }
%error { parseError }
%lexer { lexer } { EOF }
%monad { PM }

%token

    id              { UnqualId $$ }
    qualid          { QualId $$ }
    nsid            { ModuleId $$ }

    let             { KWLet }
    in              { KWIn }
    import          { KWImport }
    algo            { KWAlgo }
    sf              { KWSf }
    module          { KWModule }
    if              { KWIf }
    then            { KWThen }
    else            { KWElse }
    '('             { LParen }
    ')'             { RParen }
    '{'             { LBrace }
    '}'             { RBrace }
    '='             { OPEq }
    ':'             { OPColon }
    ';'             { OPSemicolon }
    ';;'            { OPDoubleSemicolon }
    ','             { OPComma }
    '->'            { OPArrow }
    '\\'            { OPBackslash }

%%

many1 (p)
    : p many1(p) { $1 : $2 }
    | p          { [$1] }

many (p)
    : many1(p)  { $1 }
    |           { [] }

many_sep1(p, sep)
    : p sep many_sep1(p, sep) { $1 : $3 }
    | p                       { [$1] }

many_sep(p, sep)
    : many_sep1(p, sep) { $1 }
    |                   { [] }

opt(p)
    : p { Just $1 }
    |   { Nothing }

or(a, b)
    : a { Left $1 }
    | b { Right $1 }

SomeId :: { SomeBinding }
SomeId
    : id     { Unqual $1 }
    | qualid { Qual $1 }

ModId :: { NSRef }
ModId
    : id    { makeThrow [$1] :: NSRef }
    | nsid  { $1 }

Module :: { Module }
Module
    : ModHeader many(Import) many(Decl) { ($1, $2, $3) }

ModHeader :: { ModHeader }
ModHeader
    : module ModId { $2 }

Import :: { Import }
Import
    : import ImportType ModId opt(Refers) opt(';;') { $2 ($3, fromMaybe [] $4) }

Refers :: { [Binding] }
    : '(' many_sep(id, ',') ')' { $2 }

ImportType :: { (NSRef, [Binding]) -> Import }
ImportType
    : algo { Right }
    | sf   { Left }

Decl :: { Decl }
Decl
    : ValDecl { $1 }

ValDecl :: { ValDecl }
ValDecl
    : let LetRhs '=' Exp ';;' { case $2 of Left xs -> error $ "Destructuring not allowed for top level bindings: " <> show xs; Right (bnd, f) -> (bnd, f $4) }

SimpleExp :: { Exp }
SimpleExp
    : '(' TupleOrExp ')' { $2 }
    | SomeId             { Var $1 }

Exp :: { Exp }
Exp
    : Exp SimpleExp            { Apply $1 $2 }
    | '\\' many1(Pat) '->' Exp { foldr' Lambda $4 $2 }
    | let Let in Exp           { $2 $4 }
    | if Exp then Exp else Exp { Refs.ifBuiltin `Apply` $2 `Apply` ignoreArgLambda $4 `Apply` ignoreArgLambda $6 }
    | Exp ';' Exp              { ignoreArgLet $1 $3 }
    | SimpleExp                { $1 }

Let :: { Exp -> Exp }
Let
    : LetRhs '=' Exp { case $1 of Left xs -> Let xs $3; Right (bnd, f) -> Let (Direct bnd) (f $3) }

LetRhs :: { Either Assignment (Binding, Exp -> Exp) }
LetRhs
    : Destructure  { Left $1 }
    | id many(Pat) { Right ($1, \a -> foldr' Lambda a $2) }

TupleOrExp :: { Exp }
TupleOrExp
    : many_sep(Exp, ',') { case $1 of [x] -> x; xs -> foldl' Apply (Var $ Qual Refs.mkTuple) xs }

Pat :: { Pat }
Pat
    : Destructure { $1 }
    | id          { Direct $1 }

Destructure :: { Pat }
    : '(' many_sep(id, ',') ')' { case $2 of [x] -> Direct x; xs -> Destructure xs }

{

type Decl = ValDecl
type ValDecl = (Binding, Exp)
type Module = (ModHeader, [Import], [Decl])
type Import = Either (NSRef, [Binding]) (NSRef, [Binding])
type ModHeader = NSRef
type Exp = Expr SomeBinding
type Pat = Assignment
type PM = Alex

ignoreArgLambda = Lambda (Direct "_")
ignoreArgLet = Let (Direct "_")


nextToken :: PM Lexeme
nextToken = alexMonadScan

lexer :: (Lexeme -> PM a) -> PM a
lexer cont = nextToken >>= cont

runPM :: PM a -> Input -> a
runPM ac bs = either (error . toText) identity $ runAlex bs ac

parseError :: Lexeme -> PM a
parseError token = do
  (line, col) <- getLexerPos
  alexError $ ("Parse error at line " <> show line <> ", column " <> show col <> ", on token " <> show token :: String)

parseExp :: Input -> Expr SomeBinding
parseExp = runPM parseExpRaw


-- | Parse a stream of tokens into a namespace
parseMod :: Input -> Namespace (Expr SomeBinding)
parseMod = f . runPM parseModRaw
  where
    f (name, imports, decls0) = (emptyNamespace name :: Namespace SomeBinding)
      & algoImports .~ algoRequires
      & sfImports .~ sfRequires
      & decls .~ HM.fromList decls0
      where
        (sfRequires, algoRequires) = partitionEithers imports
}
