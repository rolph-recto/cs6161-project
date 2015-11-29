module MaxSAT (

) where

import Data.RVar
import Data.Random
import Data.Random.Source.IO
import Data.Random.Extras (choice, choices)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (intercalate)
import Control.Monad

type Model = M.Map String Bool

data Formula =
  -- literals
    Sym String
  | NotSym String
  -- complex formulae
  | Or [Formula]
  | And [Formula]
  | Not Formula
  | If Formula Formula
  | Iff Formula Formula

instance Show Formula where
  show (Sym s) = s
  show (NotSym s) = "~" ++ s
  show (Or disjuncts) = "(" ++ (intercalate " || " (map show disjuncts)) ++ ")"
  show (And conjuncts) = "(" ++ (intercalate " && " (map show conjuncts)) ++ ")"
  show (Not f) = "~" ++ (show f)
  show (If p q) = "(" ++ (show p) ++ " -> " ++ (show q) ++ ")"
  show (Iff p q) = "(" ++ (show p) ++ " <-> " ++ (show q) ++ ")"

-- return the variable of a literal
litVar :: Formula -> String
litVar (Sym s) = s
litVar (NotSym s) = s
litVar _ = error "Not a literal"

-- check if a formula is satisfiable within a model
-- if symbol is not found in formula, arbitrarily return False
sat :: Model -> Formula -> Bool
sat model (Sym s) = maybe False id (s `M.lookup` model)
sat model (NotSym s) = not $ maybe False id (s `M.lookup` model)
sat model (Or disjuncts) = any (sat model) disjuncts
sat model (And conjuncts) = all (sat model) conjuncts
sat model (Not f) = not $ sat model f
sat model (If p q) = (not $ sat model p) || (sat model q)
sat model (Iff p q) = (sat model p) == (sat model q)

-- take a set of variables and generate a CNF formula
genCNF :: [String] -> Int -> Int -> IO Formula
genCNF vars nliterals nclauses = do
  clauses <- forM [1..nclauses] $ \_ -> do
    -- generate a single clause by randomly choosing variables
    -- and literal polarity (i.e., X or ~X)
    cvars <- sampleRVar $ choices nliterals vars
    clits <- sampleRVar $ choices nliterals [Sym, NotSym]
    let clause = map (\(l, s) -> l s) $ zip clits cvars
    return $ Or clause

  return $ And clauses

-- Randomized algorithm for MaxSAT
-- from Kleinberg-Tardos (chapter ??)
-- create a model by assigning T/F randomly to variables
maxSAT_KT :: [String] -> IO Model
maxSAT_KT vars = do
  vals <- sampleRVar $ choices (length vars) [True, False]
  return $ M.fromList $ zip vars vals

-- take a MaxSAT algorithm output (model)
-- and return the proportion of clauses satisfied
-- returns (m,n) where m is number of clauses satisfied and
-- n is the total number of clauses
maxSAT :: ([String] -> IO Model) -> Formula -> IO (Int,Int)
maxSAT algo (And clauses) = do
  -- collect vars in the formula
  let vars = S.toList $ foldr (\(Or lits) acc -> foldr (\l acc2 -> S.insert (litVar l) acc2) acc lits) S.empty clauses
  model <- algo vars
  let satClauses = filter (sat model) clauses
  return (length satClauses, length clauses)
maxSAT _ _ = error "Not in CNF form!"

-- return a set of variables
vars :: Int -> [String]
vars n = map (\i -> "x" ++ (show i)) [1..n]

main = do
  f <- genCNF (vars 5) 1 1000
  (m,n) <- maxSAT (maxSAT_KT) f
  print f
  print (m,n)
  
