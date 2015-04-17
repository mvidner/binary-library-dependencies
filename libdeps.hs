module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)
import Control.Arrow (second)
import Control.Monad (forM_)
import System.Environment (getArgs)

import LibSymbol

type LibName = String
type SymName = String

-- | Adjacency representation of a directed graph:
--   vertices reachable from a vertex
-- (for now, ignore Data.Graph and Data.Graph.Wrapper)
type Graph vertex = M.Map vertex (S.Set vertex)

--prettyPrint :: Show vertex => Graph vertex -> String
prettyPrint :: Graph String -> String
prettyPrint g =
    M.foldlWithKey' ppVertex "" g
  where
    ppVertex stringSoFar v adjacentVs =
        stringSoFar ++ (look v) ++ "\n" ++ (ppAdjacent adjacentVs)
    ppAdjacent vSet = S.foldl' addAdjacent "" vSet
    addAdjacent s w = s ++ "    " ++ (look w) ++ "\n"
    -- for generic Show, we need look = show,
    -- but then we'd have "quoted" "strings" "everywhere"
    look = id

type LibSymbols = M.Map LibName [LibSymbol]
type SymProvider = M.Map SymName LibName

requiredSymsToLibSet :: SymProvider -> [SymName] -> S.Set LibName
requiredSymsToLibSet symProvider syms =
    let libs = mapMaybe (flip M.lookup $ symProvider) syms :: [LibName]
    in S.fromList libs

dependencies :: SymProvider -> LibSymbols -> Graph LibName
dependencies symProvider lsMap =
    let al = M.toList lsMap                         :: [(LibName,[LibSymbol])]
        reqSymNamesAl = map (second requiredNames) al::[(LibName,[SymName])]
        reqLibNamesAl = map (second $ requiredSymsToLibSet symProvider) reqSymNamesAl ::[(LibName, S.Set LibName)]
    in M.fromList reqLibNamesAl

requiredNames :: [LibSymbol] -> [SymName]
requiredNames = map symName . filter symIsRequired

providedNames :: [LibSymbol] -> [SymName]
providedNames = map symName . filter symIsProvided

explode :: (a,[b]) -> [(a,b)]
explode (a, bs) = map (\b -> (a,b)) bs

toProviderNamesAl :: LibSymbols -> [(LibName, SymName)]
toProviderNamesAl lsMap =
    let al = M.toList lsMap                         :: [(LibName,[LibSymbol])]
        provNamesAl = map (second providedNames) al :: [(LibName,[SymName])]
    in concat $ map explode provNamesAl

toProvider :: LibSymbols -> SymProvider
toProvider lsMap =
    M.fromList $ map swap $ toProviderNamesAl lsMap

toProviders :: LibSymbols -> M.Map SymName [LibName]
toProviders =
    M.fromListWith (++) . map (\(l,s)->(s,[l])) . toProviderNamesAl

-- ===== statistics =====
histogram :: Ord a => [a] -> [(a, Int)]
histogram xs =
    let xAl = map (\x -> (x,1)) xs
        histMap = M.fromListWith (+) xAl
    in M.toList histMap

-- | For a multimap, report [(size,n)] :
--   values that were size-fold occurred n times
bucketSizes :: M.Map a [b] -> [(Int,Int)]
bucketSizes m = map swap $ histogram $ map length $ M.elems m

-- ===== main =====
main :: IO ()
main = do
    libFileNames <- getArgs
    allLibSymbolsL <- mapM readLibrarySymbols libFileNames
    let allLibSymbols = M.fromList $ zip libFileNames allLibSymbolsL
    let provider = toProvider allLibSymbols
    let providers = toProviders allLibSymbols
    putStrLn $ show (bucketSizes providers) ++ " symbols provided"
    putStrLn "Dependencies:"
    putStrLn $ prettyPrint $ dependencies provider allLibSymbols
