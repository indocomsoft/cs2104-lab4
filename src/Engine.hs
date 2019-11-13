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

-- Status in the tree:
--   - Continue is the default case
--   - Stop is when a cut is reached, it also records at which height the rule
--     containing the Cut originates from
--   - Backtrack is an internal state that should not leak outside of searchAll,
--     only for when a rule containing a Cut needs to backtrack to parent clause
--     which must be done without proceeding to the next matching rules of the
--     current query.
data Status = Continue [Subs] | Stop [Subs] Int | Backtrack [Subs] deriving Show

-- For each Rel, marks at which height the Rule containing this Rel originates
data RelMetadata = RelMetadata Int Rel deriving Show


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
startSearch (Program rules) query =
  case searchAll rules [RelMetadata 0 query] M.empty 0 of
    Continue  subs -> subs
    Backtrack subs -> subs
    Stop subs _    -> subs

searchAll :: [Rule] -> [RelMetadata] -> Subs -> Int -> Status
searchAll rules asd@((RelMetadata level Cut) : queries) subs height =
  -- Upon reaching cut, change Status to Stop.
  -- If there are more than one Cut in the queries, the one originating from
  -- the lower height (nearer to the root) prevails
  case searchAll rules queries subs (height + 1) of
    Continue subs      -> Stop subs level
    Stop subs newLevel -> Stop subs (min level newLevel)
searchAll rules asd@(query : queries) subs height =
  let
    RelMetadata _ queryRel = query
    matchingRules          = filter (match queryRel) rules
    queryTerm              = toFunctor queryRel
    process rule =
      let
        Rule head body = rename rule height
        ruleTerm       = toFunctor head
        newQueries     = map (RelMetadata height) (concat body) ++ queries
      in case unify queryTerm ruleTerm subs of
        Nothing      -> Continue []
        Just newSubs -> if null newQueries
          then Continue [newSubs]
          else case searchAll rules newQueries newSubs (height + 1) of
            Continue resultSubs -> Continue resultSubs
            status@(Stop resultSubs level) ->
              -- If the cut originates from this level,
              -- then backtrack to parent clause, otherwise propagate status
              if level == height then Backtrack resultSubs else status
  in case interruptibleProcessRules process matchingRules of
    Backtrack resultSubs -> Continue resultSubs
    other                -> other

interruptibleProcessRules :: (Rule -> Status) -> [Rule] -> Status
interruptibleProcessRules f []       = Continue []
interruptibleProcessRules f (x : xs) = case f x of
  Continue ys -> case interruptibleProcessRules f xs of
    Continue zs   -> Continue (ys ++ zs)
    Stop zs level -> Stop (ys ++ zs) level
    Backtrack zs  -> Backtrack (ys ++ zs)
  other -> other

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
