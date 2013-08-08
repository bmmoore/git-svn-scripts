{-# LANGUAGE QuasiQuotes, ViewPatterns,LambdaCase, TypeFamilies,
    ScopedTypeVariables, Rank2Types #-}
module MkIgnore where
import Data.List
import Data.List.Split
import qualified Data.DList as D

import Data.Tree
import Data.Functor.Foldable
import Control.Comonad
import Control.Comonad.Env
import Control.Comonad.Cofree
import Control.Monad.Writer
import Data.Functor.Compose
import Data.Monoid
import Control.Applicative
import Data.Maybe
import qualified Data.Map as Map

import Here(here)

-- Generic trees with labeled edges
type LTree edge node = Cofree (Map.Map edge) node

-- If node labels are Maybe, prune subtrees containing only Nothing
pruneEmpty' :: LTree edge (Maybe a) -> Maybe (LTree edge (Maybe a))
pruneEmpty' (n :< children) =
  let children' = Map.mapMaybe pruneEmpty' children
  in returnUnless (isNothing n && Map.null children') (n :< children')

pruneEmpty :: LTree edge (Maybe a) -> LTree edge (Maybe a)
pruneEmpty = maybe (Nothing :< Map.empty) id . pruneEmpty'

-- Keep only nodes where trees differ,
-- or the set of child labels are not the same
diff :: (Eq node, Ord edge) =>
        LTree edge node -> LTree edge node
        -> LTree edge (Maybe (Maybe(node,node),[edge],[edge]))
diff (k1 :< m1) (k2 :< m2) =
  returnUnless (k1 == k2 && Map.keysSet m1 == Map.keysSet m2)
    (returnUnless (k1 == k2) (k1,k2),Map.keys (Map.difference m1 m2),Map.keys (Map.difference m2 m1))
  :< Map.intersectionWith diff m1 m2

-- Foldable and Unfoldable instances for cofree
type instance Base (Cofree f a) = EnvT a f
instance Functor f => Foldable (Cofree f a) where
  project (a :< v) = EnvT a v
instance Functor f => Unfoldable (Cofree f a) where
  embed (EnvT a v) = (a :< v)

-- degenerate tree from a label path and value
pathTree :: [ix] -> a -> LTree ix (Maybe a)
pathTree path v = foldr (\ix t -> Nothing :< Map.singleton ix t) (Just v :< Map.empty) path

-- union two trees, given combining function
mergeTreeWith :: (Ord edge) => (a -> a -> a) -> LTree edge a -> LTree edge a -> LTree edge a
mergeTreeWith merge (a1 :< ts1) (a2 :< ts2)
  = merge a1 a2 :< Map.unionWith (mergeTreeWith merge) ts1 ts2

-- union a set of trees, given monoid instance
mergeTrees :: (Monoid a, Ord edge) => [LTree edge a] -> LTree edge a
mergeTrees trees = mconcat (map extract trees)
                   :< Map.unionsWith (mergeTreeWith mappend) (map unwrap trees)

-- distribution functions for recursion schemees
distLabel :: (Monoid m) => (ix -> m) -> Map.Map ix (m -> a) -> (m -> Map.Map ix a)
distLabel ixVal items m = Map.mapWithKey (\ix v -> v (m <> ixVal ix)) items

distEnvT :: (Comonad w) => (forall b . f (w b) -> w (f b)) ->
                           EnvT e f (w a) -> w (EnvT e f a)
distEnvT distInner (EnvT e children) = fmap (EnvT e) (distInner children)

distEnv :: (Functor f) => EnvT e f (m -> a) -> (m -> EnvT e f a)
distEnv (EnvT e v) m = EnvT e (fmap ($ m) v)

-- turn a tree back into a set of path-labeled values
untree :: LTree ix a -> [([ix],a)]
untree tree = D.toList $ cata (ap finish . dist) tree D.empty
  where dist = distEnvT (distLabel D.singleton)
        finish path (EnvT node children) = D.cons (D.toList path,node)
                                           (D.concat (Map.elems children))
-- maybe helpers
returnUnless True _ = mzero
returnUnless False x = return x

returnSatisfying pred v | pred v = return v
                        | otherwise = mzero

-- map taking a list of ancestor values
treeMapMaybeWithAncestors :: ([a] -> a -> Maybe b)
                       -> LTree edge a -> Maybe (LTree edge (Maybe b))
treeMapMaybeWithAncestors f = go [] where
  go path (a :< children) =
    let b = f path a
        children' = Map.mapMaybe (go (a:path)) children
    in returnUnless (isNothing b && Map.null children') (b :< children')

-- more complicated map, with
-- ancestor node and edge labels summarized into some kind of context
whee :: (a -> edge -> ctx -> ctx) ->
        (ctx -> a -> Maybe b) ->
        LTree edge a -> ctx -> Maybe (LTree edge (Maybe b))
whee advanceCtx f = go where
  go (a :< children) ctx =
    let b = f ctx a
        children' = Map.mapMaybeWithKey (\edge child -> go child (advanceCtx a edge ctx)) children
        nonEmpty (Nothing :< (Map.null -> True)) = False
        nonEmpty _ = True
    in returnSatisfying nonEmpty (b :< children')

-- Representing the set of active .gitingnore globs at a point.
-- fst is the set of single-component patterns, which apply recursively
-- snd is the set of multiple-component patterns, which only apply
-- in corresponding subdirectories
type Ignores = ([String],[[String]])

-- match a glob against a string
glob :: String -> String -> Bool
glob ('*':pat) str = any (glob pat) (tails str)
glob ('?':pat) (_:str) = glob pat str
glob ('?':_) _ = False
glob ('\\':c:pat) (c':str) = c == c' && glob pat str
glob (c:pat) (c':str) = c == c' && glob pat str
glob [] (_:_) = False
glob (_:_) [] = False
glob [] [] = True

-- Add a set of patterns to an Ignores represntation
addPats :: Maybe [String] -> Ignores -> Ignores
addPats Nothing x = x
addPats (Just pats) x = foldr addPat x pats

-- add a single pattern, deciding whether it's anchored or not.
-- skip patterns implied by existing ignores
addPat :: String -> Ignores -> Ignores
addPat pat ignores | globSubsumes ignores pat = ignores
addPat pat (globs,anchored) =
  case splitOn "/" pat of
    [_] -> (pat:globs,anchored)
    [_,""] -> (pat:globs,anchored)
    pieces -> (globs,pieces:anchored)

-- Compute set of ignores active in given subdirectory
advanceDir :: String -> Ignores -> Ignores
advanceDir dir (globs,anchored)
  = (globs, [pat | (first:pat) <- anchored, glob first dir])

advanceCtx :: Maybe [String] -> String -> Ignores -> Ignores
advanceCtx pats dir ignores = advanceDir dir (addPats pats ignores)

-- Approximate whether a set of ignores subsumes a given pattern
globSubsumes :: Ignores -> String -> Bool
globSubsumes (free,paths) str =
  not ('/' `elem` str) && or [globSupset f str | f <- free]
  || (splitOn "/" str) `elem` paths

-- approximate, doesn't try to handle
-- muliple *
globSupset bigGlob littleGlob =
  bigGlob == littleGlob
  || (null (intersect "?*\\" littleGlob)
      && glob bigGlob littleGlob)

-- Filter ignores at a node, given
-- information on the inherited ignores
filterIgnores _ Nothing = Nothing
filterIgnores active (Just patterns) =
  let remaining = filter (not . globSubsumes active) patterns
  in returnSatisfying (not . null) remaining

-- remove redundant ignores from a tree
reduceTree tree = whee advanceCtx filterIgnores tree ([],[])

-- test whether we get the same blocks from treeing and untreeing.
-- input must not have multiple blocks for the same path
testUntree blocks =
  sort (mapMaybe (\(p,v) -> (,) p <$> v) (untree (treeOfBlocks blocks)))
  ==
  sort blocks

showTree' tree = unlines [intercalate "/" path++" - "++show pats | (path,Just pats) <- untree tree]

showTree tree = unlines [intercalate "/" path++" - "++unlines pats | (path,Just pats) <- untree tree]

parseBlock ((splitOn " - " -> [dir,item]):rest) = (pathParts dir,item:rest)

pathParts "." = []
pathParts p = splitOn "/" p

treeOfBlocks = mergeTrees . map (uncurry pathTree)
parseBlocks = map parseBlock . splitOn [""] . lines

parseIgnores = treeOfBlocks . parseBlocks
parseFile = liftM parseIgnores . readFile

ignoresSubsume treeIgnores treeTest = do
  let Just t1 = reduceTree treeIgnores
      Just t2 = reduceTree (mergeTrees [treeTest,treeIgnores])
      norm1 = pruneEmpty $ fmap (fmap (nub . sort)) t1
      norm2 = pruneEmpty $ fmap (fmap (nub . sort)) t2
  putStrLn "Norm1"
  putStr . showTree' $ norm1
  putStrLn "Norm2"  
  putStr . showTree' $ norm2
  putStrLn "Diff"
  putStr . showTree' $ diff norm1 norm2  

main = do
  t1 <- parseFile "newignores.txt"
  t2 <- parseFile "ignores.txt"
  ignoresSubsume t1 (mergeTrees [t1,t2])
