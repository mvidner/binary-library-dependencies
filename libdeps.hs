module Main where

import qualified Data.Map as M
import qualified Data.Set as S
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

type LibSymbols = M.Map LibName [LibSymbol]

{-
libraryProvidingSymbol :: SymName -> LibName
libraryProvidingSymbol s =

    
dependencies :: LibSymbols -> Graph LibName
dependencies lsMap =
-}

explode :: (a,[b]) -> [(a,b)]
explode (a, bs) = map (\b -> (a,b)) bs

providedNames :: [LibSymbol] -> [SymName]
providedNames = map symName . filter symIsProvided

toProviderNamesAl :: LibSymbols -> [(LibName, SymName)]
toProviderNamesAl lsMap =
    let al = M.toList lsMap                         :: [(LibName,[LibSymbol])]
        provNamesAl = map (second providedNames) al :: [(LibName,[SymName])]
    in concat $ map explode provNamesAl

toProvider :: LibSymbols -> M.Map SymName LibName
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

bucketSizes :: M.Map a [b] -> [(Int,Int)]
bucketSizes m = map swap $ histogram $ map length $ M.elems m

-- ===== main =====
main :: IO ()
main = do
    libFileNames <- getArgs
    allLibSymbolsL <- mapM readLibrarySymbols libFileNames
    let allLibSymbols = M.fromList $ zip libFileNames allLibSymbolsL
    let providers = toProviders allLibSymbols
    putStrLn $ show (bucketSizes providers) ++ " symbols provided"
