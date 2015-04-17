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


toProvider :: LibSymbols -> M.Map SymName LibName
toProvider lsMap =
    let unpacked = concat $ map explode $ M.toList lsMap ::[(LibName,LibSymbol)]
        provided = filter (symIsProvided . snd) unpacked
        providedNames = map (second symName) provided ::[(LibName,SymName)]
    in M.fromList $ map swap providedNames

-- toProviders :: LibSymbols -> M.Map SymName [LibName]
-- toProviders lsMap = 

main :: IO ()
main = do
    libFileNames <- getArgs
    allLibSymbolsL <- mapM readLibrarySymbols libFileNames
    let allLibSymbols = M.fromList $ zip libFileNames allLibSymbolsL
    let provider = toProvider allLibSymbols
    putStrLn $ show (M.size provider) ++ " symbols provided"
{-
    
    forM_ libFileNames $ \lN -> do
        syms <- readLibrarySymbols lN
        print (lN, length syms)
        putStrLn " sample:"
        print $ take 5 syms
-}
