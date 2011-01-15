{-# LANGUAGE PatternGuards #-}

module Fist.Interactive where

import qualified System.Console.Readline as RL
import qualified Fist.Syntax as Syntax
import qualified Text.Parsec as P
import qualified Data.Map as Map
import Control.Monad.Reader
import Data.Functor.Identity
import Fist.Value
import Control.Applicative

repl :: Module -> IO ()
repl mod = do
    mline <- RL.readline "<\\> "
    case mline of
        Nothing -> return ()  -- XXX save
        Just line -> 
            case runReader (P.runPT (parser <* P.eof) () "<input>" line) resolver of
                Left err -> print err >> repl mod
                Right x -> x >>= repl
    where
    parser = (\(n,v) -> return (Map.insert n v mod))              <$> P.try Syntax.defn
         <|> (\e -> print (Syntax.pretty (eval e)) >> return mod) <$> Syntax.expr
         <|> (\() -> return mod)                                  <$> Syntax.whitespace
    resolver = Syntax.Resolver resolveVar underLambda
    resolveVar v | Just m <- Map.lookup (Symbol v) mod = EApp (EVar (Variable "this")) (EPrim (PSym (Symbol v)))
                 | otherwise = EVar (Variable v)
    underLambda n = resolver  -- XXX wrong
    
    fix = ELam (Variable "f") $ EApp (ELam (Variable "x") (EApp (EVar (Variable "x")) (EVar (Variable "x"))))
                                     (ELam (Variable "x") (EApp (EVar (Variable "f")) (EApp (EVar (Variable "x")) (EVar (Variable "x")))))

    eval e = runIdentity . whnf $ EApp (ELam (Variable "this") e) (EApp fix (ELam (Variable "this") (EMod mod)))
