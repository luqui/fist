{-# LANGUAGE PatternGuards #-}

module Fist.Value 
    ( MonadEval(..)
    , Symbol(..)
    , Variable(..)
    , Module
    , Prim(..)
    , Exp(..)
    , whnf
    ) 
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (inits, tails)
import Control.Applicative
import Data.Char (isDigit)
import Data.Functor.Identity


class Monad m => MonadEval m where
instance MonadEval Identity

newtype Symbol = Symbol String
    deriving (Eq,Ord,Show)

newtype Variable = Variable String
    deriving (Eq,Ord,Show)

type Module = Map.Map Symbol Exp

data Prim
    = PInt Integer
    | PStr String
    | PSym Symbol
    deriving (Eq,Show)

data Exp
    = EPrim Prim
    | EVar Variable
    | EApp Exp Exp
    | ELam Variable Exp
    | EMod Module
    deriving (Eq,Show)

whnf :: (MonadEval m) => Exp -> m Exp
whnf (EApp f x) = do
    fnf <- whnf f
    case fnf of
        EMod m -> do
            xnf <- whnf x
            case xnf of
                EPrim (PSym p) | Just v <- Map.lookup p m -> whnf v
                e -> return $ EApp (EMod m) e
        EVar v -> return $ EApp (EVar v) x
        ELam v body -> whnf $ substitute v x body
        z -> return $ EApp z x
whnf x = return x

substitute :: Variable -> Exp -> Exp -> Exp
substitute v with = go
    where
    withFVs = freeVars with
    
    go (EPrim p) = EPrim p
    go (EVar v') | v == v' = with
                 | otherwise = EVar v'
    go (EApp t u) = EApp (go t) (go u)
    go l@(ELam v' body) | v == v' = l
                        | v' `Set.member` withFVs = go (alphaConvertFresh withFVs l)
                        | otherwise = ELam v' (go body)
    go (EMod m) = EMod (Map.map go m)

freeVars :: Exp -> Set.Set Variable
freeVars (EPrim _) = Set.empty
freeVars (EVar v) = Set.singleton v
freeVars (EApp t u) = Set.union (freeVars t) (freeVars u)
freeVars (ELam v e) = Set.delete v (freeVars e)
freeVars (EMod m)   = Set.unions . map freeVars . Map.elems $ m

alphaConvertFresh :: Set.Set Variable -> Exp -> Exp
alphaConvertFresh invars (ELam v body) = ELam v' (substitute v (EVar v') body)
    where
    avoid = invars `Set.union` freeVars body
    v' = freshVar avoid v
alphaConvertFresh _ _ = error "Alpha conversion only applies to lambdas"

freshVar :: Set.Set Variable -> Variable -> Variable
freshVar avoid v@(Variable name) = head . filter (not . (`Set.member` avoid)) . map (Variable . (pfx ++) . show) $ [sfx..]
    where
    (pfx,sfx) = head [ (a, read' b) | (a,b) <- splits name, all isDigit b ]
    read' "" = 0
    read' n  = read n

splits :: [a] -> [([a],[a])]
splits = liftA2 zip inits tails

