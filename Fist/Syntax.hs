{-# LANGUAGE NoMonomorphismRestriction #-}

module Fist.Syntax where

import Prelude hiding (lex)
import qualified Data.Map as Map
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as P
import qualified Text.PrettyPrint.HughesPJ as PP
import Control.Applicative
import Fist.Value

pretty :: Exp -> PP.Doc
pretty (EPrim (PInt z)) = PP.integer z
pretty (EPrim (PStr s)) = PP.text (show s)
pretty (EPrim (PSym (Symbol s))) = PP.char '\'' PP.<> PP.text s
pretty (EVar (Variable v)) = PP.text v
pretty (EApp t u) = parensL t (pretty t) PP.<+> parensR u (pretty u)
    where
    parensL (ELam {}) = PP.parens
    parensL _ = id
    parensR (ELam {}) = PP.parens
    parensR (EApp {}) = PP.parens
    parensR _ = id
pretty (ELam (Variable v) b) = PP.char '\\' PP.<> PP.text v PP.<+> PP.text "->" PP.<+> pretty b
pretty (EMod m) = PP.braces . PP.sep . PP.punctuate (PP.text ",") . map ppEntry . Map.assocs $ m
    where
    ppEntry (Symbol k,v) = PP.text k PP.<+> PP.text "=" PP.<+> pretty v

lex = P.makeTokenParser $ P.LanguageDef {
    P.commentStart = "{-",
    P.commentEnd = "-}",
    P.commentLine = "--",
    P.nestedComments = True,
    P.identStart = P.letter <|> P.char '_',
    P.identLetter = P.alphaNum <|> P.char '_',
    P.opStart = fail "no operators",
    P.opLetter = fail "no operators",
    P.reservedNames = [],
    P.reservedOpNames = ["->", "=", ",", "\\"],
    P.caseSensitive = True
}

pTerm = (EPrim <$> pPrim) <|> (EVar <$> pVar) <|> (EMod <$> pModule) <|> pLambda <|> P.parens lex pExpr
pPrim = (PInt <$> P.integer lex) <|> (PStr <$> P.stringLiteral lex) <|> (PSym <$> pSymbol)
pSymbol = Symbol <$> (P.char '\'' *> P.identifier lex)
pVar = Variable <$> P.identifier lex
pModule = Map.fromList <$> (P.braces lex $ P.commaSep lex pDefn <* optional (P.reservedOp lex ","))
pDefn = massage <$> (Symbol <$> P.identifier lex) <*> P.many pVar <* P.reservedOp lex "=" <*> pExpr
    where massage sym vars body = (sym, foldr ELam body vars)
pLambda = flip (foldr ELam) <$> (P.reservedOp lex "\\" *> P.many1 pVar <* P.reservedOp lex "->") <*> pExpr
pExpr = foldl1 EApp <$> P.many1 pTerm

parseExp :: String -> Either P.ParseError Exp
parseExp = P.parse pExpr "<input>"

parseDefn :: String -> Either P.ParseError (Symbol, Exp)
parseDefn = P.parse pDefn "<input>"
