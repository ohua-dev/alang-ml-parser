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

import Control.Lens (view)
import Ohua.Compat.ML.Lexer
import Ohua.Frontend.Lang
import Ohua.Frontend.NS
import qualified Data.HashMap.Strict as HM
import qualified Ohua.ParseTools.Refs as Refs
import qualified Ohua.ALang.Refs as Refs
import Ohua.Unit (someUnitExpr)
import Data.List.NonEmpty (NonEmpty((:|)))

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
    envRef          { EnvRef $$ }
    number          { Number $$ }

    let             { KWLet }
    in              { KWIn }
    import          { KWImport }
    algo            { KWAlgo }
    sf              { KWSf }
    module          { KWModule }
    if              { KWIf }
    then            { KWThen }
    else            { KWElse }
    with            { KWWith }
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
    'λ'             { OPLambda }
    '-'             { OPMinus }
    '_'             { UnqualId "_" }

%%

many1 (p)
    : p many(p) { $1 :| $2 }

many (p)
    : p many(p)  { $1 : $2 }
    |            { [] }

many_sep1(p, sep)
    : p sep many_sep1(p, sep) { let x :| xs = $3 in $1 :| x:xs }
    | p                       { $1 :| [] }

many_sep(p, sep)
    : many_sep1(p, sep) { toList $1 }
    |                   { [] }

opt(p)
    : p { Just $1 }
    |   { Nothing }

or(a, b)
    : a { Left $1 }
    | b { Right $1 }

ModId
    :: { NSRef }
    : id    { makeThrow [$1] :: NSRef }
    | nsid  { $1 }

Module
    :: { Module }
    : ModHeader many(Import) many(Decl) { ($1, $2, $3) }

ModHeader
    :: { ModHeader }
    : module ModId { $2 }

Import
    :: { Import }
    : import ImportType ModId opt(Refers) opt(';;') { ($2, $3, fromMaybe [] $4) }

Refers :: { [Binding] }
    : '(' many_sep(id, ',') ')' { $2 }

ImportType
    :: { Bool }
    : algo { True }
    | sf   { False }

Decl
    :: { Decl }
    : ValDecl { $1 }

ValDecl
    :: { ValDecl }
    : let LetRhs '=' Exp ';;' { let (pat1, f) = $2 in
                                case pat1 of
                                    VarP bnd -> (bnd, f $4)
                                    xs -> error $ "Non-var patterns not allowed for top level bindings: " <> show xs }

SimpleExp
    :: { Exp }
    : '(' many_sep(Exp, ',') ')' { case $2 of
                                       [] -> LitE UnitLit
                                       [x] -> x
                                       xs -> TupE xs }
    | opt('-') number            { LitE $ NumericLit $ maybe id (const negate) $1 $2 }
    | envRef                     { LitE $ EnvRefLit $1 }
    | qualid                     { LitE $ FunRefLit $ FunRef $1 Nothing }
    | id                         { VarE $1 }

Exp :: { Exp }
    : Exp SimpleExp            { AppE $1 [$2] }
    | 'λ' many1(Pat) '->' Exp  { LamE (toList $2) $4 }
    | let Let in Exp           { $2 $4 }
    | if Exp then Exp else Exp { IfE $2 $4 $6 }
    | Exp ';' Exp              { StmtE $1 $3 }
    | Exp with Exp             { BindE $1 $3 }
    | SimpleExp                { $1 }

Let :: { Exp -> Exp }
    : LetRhs '=' Exp { let (xs, f) = $1 in LetE xs $ f $3 }

LetRhs
    :: { (Pat, Exp -> Exp) }
    : many1(Pat) { let x :| xs = $1 in (x, \a -> if null xs then a else LamE xs a) }

Pat :: { Pat }
    : '(' many_sep(Pat, ',') ')' { case $2 of
                                       [] -> UnitP
                                       [x] -> x
                                       x:xs -> TupP $ x : xs }
    | id          { VarP $1 }

{

type Decl = ValDecl
type ValDecl = (Binding, Exp)
type Module = (ModHeader, [Import], [Decl])
type Import = (Bool, NSRef, [Binding])
type ModHeader = NSRef
type Exp = Expr
type PM = Alex

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

parseExp :: Input -> Expr
parseExp = runPM parseExpRaw


-- | Parse a stream of tokens into a namespace
parseMod :: Input -> Namespace Expr
parseMod = f . runPM parseModRaw
  where
    f (name, imports, decls0) = (emptyNamespace name :: Namespace ())
      & algoImports .~ algoRequires
      & sfImports .~ sfRequires
      & decls .~ HM.fromList decls0
      where
        dropImportType = map $ \(_, b, c) -> (b, c)
        sfRequires = dropImportType $ filter (not . view _1) imports
        algoRequires = dropImportType $ filter (view _1) imports
}
