module Main where

import Control.Monad (forM_)
import Control.Applicative
import System.Environment (getArgs)

-- | A library symbol, as dumped by 'nm'
data LibSymbol = LibSymbol
    { symType :: !Char
    , symName :: !String
    } deriving (Eq, Show)

type LibName = String

symIsProvided :: LibSymbol -> Bool
symIsProvided lS = symType lS == 'T'

symIsRequired :: LibSymbol -> Bool
symIsRequired lS = symType lS == 'U'

symIsInteresting :: LibSymbol -> Bool
-- symIsInteresting lS = (symIsProvided lS) || (symIsRequired lS)
symIsInteresting = (||) <$> symIsProvided <*> symIsRequired

parsePosixNmLine :: String -> LibSymbol
parsePosixNmLine l =
    LibSymbol { symType = t, symName = n }
  where
    (n, rest) = span (/= ' ') l
    t = rest !! 1
    
-- posix format: name space type [space other-info]
parsePosixNmOutput :: String -> [LibSymbol]
parsePosixNmOutput s = map parsePosixNmLine $ lines s

fakeParsePosixNmOutput :: String -> [LibSymbol]
fakeParsePosixNmOutput _ =
    [ LibSymbol { symType = 'U', symName = "IRequire" }
    , LibSymbol { symType = 'T', symName = "IProvide" }
    ]

getLibrarySymbols :: LibName -> IO [LibSymbol]
getLibrarySymbols fileName = do
    out <- runNm fileName
    return $ parsePosixNmOutput out

runNm :: LibName -> IO String
runNm fileName = return "fakeRequire U\nfakeProvide T\n"

main :: IO ()
main = do
  libFileNames <- getArgs
  forM_ libFileNames $ \lN -> do
         syms <- getLibrarySymbols lN
         print (lN, length syms)
