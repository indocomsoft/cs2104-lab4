module Engine where

import AST
import Control.Monad
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Goal = Goal Subs [Rel]
data Tree = Tree Goal [Tree] Bool -- isExpanded
 {- this represent the resolution tree -}
type Subs = M.Map String Term
 {- this represents a substitution -}

{- an initial resolution tree -}
initTree :: Rel -> Tree
initTree rel = Tree (Goal M.empty [rel]) [] False

{- unifying two terms -}
unify :: Term -> Term -> Subs -> Maybe Subs
unify (Atom a1) (Atom a2) subs = if a1 == a2 then Just subs else Nothing
unify (Func n1 t1) (Func n2 t2) subs =
  if n1 == n2 then foldl (>=>) Just (zipWith unify t1 t2) subs else Nothing
unify (Var v) term subs = case subs M.!? v of
  Just term' -> unify term' term subs
  Nothing    -> Just (M.insert v term subs)
unify term (var@(Var v)) subs = unify var term subs
unify _    _             _    = Nothing

{- renaming a clause -}
rename :: Rule -> Int -> Rule
rename (Rule head body) height = Rule
  (renameRel head)
  (map (map renameRel) body)
 where
  renameRel Cut = Cut
  renameRel rel = toRel . renameTerm . toFunctor $ rel
  toRel (Func name terms) = Rel name terms
  renameTerm (Var v          ) = Var (v ++ show height)
  renameTerm (Func name terms) = Func name (map renameTerm terms)
  renameTerm atom              = atom

toFunctor :: Rel -> Term
toFunctor (Rel name terms) = Func name terms

match :: Rel -> Rule -> Bool
match (Rel name terms) (Rule (Rel name' terms') _) =
  name == name' && length terms == length terms'


startSearch :: Program -> Rel -> [Subs]
startSearch (Program rules) query = case searchAll rules [query] M.empty 0 of
  Right subs -> subs
  Left  subs -> subs

searchAll :: [Rule] -> [Rel] -> Subs -> Int -> Either [Subs] [Subs]
searchAll rules (Cut : queries) subs height = -- Prevent backtracking on cut
  case searchAll rules queries subs (height + 1) of
    Right newSubs -> Left newSubs
    Left  newSubs -> Left newSubs
searchAll rules (query : queries) subs height =
  let
    matchingRules = filter (match query) rules
    process rule =
      let
        Rule head body = rename rule height
        ruleTerm       = toFunctor head
        queryTerm      = toFunctor query
        newQueries     = concat body ++ queries -- assuming no disjunction
      in case unify queryTerm ruleTerm subs of
        Nothing      -> Right []
        Just newSubs -> if null newQueries
          then Right [newSubs]
          else searchAll rules newQueries newSubs (height + 1)
  in interruptibleConcatMap process matchingRules

-- perform concatMap only as long as the return value is Right.
-- Preserves Right as long as f returns Right, once f returns Left, stop
interruptibleConcatMap :: (a -> Either [b] [b]) -> [a] -> Either [b] [b]
interruptibleConcatMap f []       = Right []
interruptibleConcatMap f (x : xs) = case f x of
  Right ys -> case interruptibleConcatMap f xs of
    Right zs -> Right $ ys ++ zs
    Left  zs -> Left $ ys ++ zs
  Left ys -> Left ys


{- returns all variables in a relation -}
variables :: Rel -> [Term]
variables rel = S.toList . S.fromList . aux . toFunctor $ rel
 where
  aux v@(Var x       ) = [v]
  aux (  Func _ terms) = terms >>= aux
  aux (  Atom _      ) = []

{- apply substitution to a term -}
resolve :: Subs -> Term -> Term
resolve subs (Var x) = case M.lookup x subs of
  Just term -> resolve subs term
  Nothing   -> Var x

resolve subs (  Func name terms) = Func name (map (resolve subs) terms)
resolve subs a@(Atom _         ) = a
