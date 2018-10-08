{
-- |
-- Module      : $Header$
-- Description : Lexer for a simple ML language
-- Copyright   : (c) Justus Adam 2018. All Rights Reserved.
-- License     : EPL-1.0
-- Maintainer  : dev@justus.science
-- Stability   : experimental

-- This source code is licensed under the terms described in the associated LICENSE.TXT file
{-# LANGUAGE BangPatterns, LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields -fno-warn-unused-imports #-}
module Ohua.Compat.ML.Lexer (tokenize, Lexeme(..), Alex, Input, getLexerPos, alexMonadScan, runAlex, alexError) where

import Ohua.Prelude hiding (undefined)

import qualified Data.ByteString.Lazy.Char8 as BS

import Prelude (undefined)
}

%wrapper "monad-bytestring"

$char = [a-zA-Z]
$sym  = [\-\>\<\$\*\+\?\~\^\=_]
$numerical = [0-9]
$reserved = [@\#\{\}\[\]]
$idstartchar = [$char]
$idchar = [$numerical $idstartchar]
$sep = [$white]

@id = $idstartchar $idchar*
@ns = @id (\. @id)*


:-

    "("             { direct LParen }
    ")"             { direct RParen }
    "let"           { direct KWLet }
    "in"            { direct KWIn }
    "if"            { direct KWIf }
    "then"          { direct KWThen }
    "else"          { direct KWElse }
    "import"        { direct KWImport }
    "algo"          { direct KWAlgo }
    "sf"            { direct KWSf }
    "module"        { direct KWModule }
    "="             { direct OPEq }
    ":"             { direct OPColon }
    ";"             { direct OPSemicolon }
    ";;"            { direct OPDoubleSemicolon }
    ","             { direct OPComma }
    "->"            { direct OPArrow }
    "\"             { direct OPBackslash }
    @id             { tokenOverInputStr $ UnqualId . convertId }
    @ns\/@id        { tokenOverInputStr $ QualId . mkQualId }
    @ns             { tokenOverInputStr $ ModuleId . mkNSRef }
    ^ "%" .* $      { tokenOverInputStr $ Pragma . decodeUtf8 . BS.drop 1 }
    $sep            ;

    $reserved       { withMatchedInput $ \s -> alexError $ "Reserved symbol: " <> decodeUtf8 s }


{
type Input = BS.ByteString

data Lexeme
    = LParen -- ^ @(@
    | RParen -- ^ @)@
    | KWLet -- ^ keyword @let@
    | KWIn -- ^ keyword @in@
    | KWIf -- ^ keyword @if@
    | KWThen -- ^ keyword @then@
    | KWElse -- ^ keyword @else@
    | KWModule -- ^ keyword @module@
    | KWImport -- ^ keyword @import@
    | KWAlgo -- ^ keyword @algo@
    | KWSf -- ^ keyword @sf@
    | OPEq -- ^ operator @=@
    | OPColon -- ^ operator @:@
    | OPSemicolon -- ^ operator @;@
    | OPDoubleSemicolon -- ^ operator @;;@
    | OPComma -- ^ operator @,@
    | OPArrow -- ^ operator @->@
    | OPBackslash -- ^ operator @\\@
    | UnqualId Binding -- ^ an identifier
    | QualId QualifiedBinding -- ^ a qualified binding
    | ModuleId NSRef -- ^ an identifier for a module
    | Pragma Text
    | EOF
    deriving Show

withMatchedInput f (_, _, input, _) len = f (BS.take len input)
tokenOverInputStr f = withMatchedInput (pure . f)

direct tok _ _ = pure tok
alexEOF = pure EOF

convertId :: ByteString.ByteString -> Binding
convertId = makeThrow . decodeUtf8

tokenize :: BS.ByteString -> [Lexeme]
tokenize bs = either (error . toText) identity $ runAlex bs $
  let go = alexMonadScan >>= \case EOF -> pure []; tok -> (tok:) <$> go
  in go

getLexerPos :: Alex (Int, Int)
getLexerPos = Alex $ \s@AlexState{ alex_pos=AlexPn _ line col} -> pure (s, (line, col))

mkQualId :: BS.ByteString -> QualifiedBinding
mkQualId str = QualifiedBinding (mkNSRef nsstr) (convertId name0)
  where
    (nsstr, name') = BS.break (== '/') str
    name0 = BS.tail name'


mkNSRef :: BS.ByteString -> NSRef
mkNSRef = makeThrow . map convertId . BS.split '.'


}
