module LibSymbol
       (
           LibSymbol,
           symName,
           symIsProvided,

           readLibrarySymbols
       ) where

import Control.Applicative
import System.Process (readProcess)


data LibSymbolType = Required
                   | Provided
                   | Other

-- | A library symbol, as dumped by 'nm'
data LibSymbol = LibSymbol
    { symType :: !Char
    , symName :: !String
    } deriving (Eq, Show)

symIsProvided :: LibSymbol -> Bool
symIsProvided lS = symType lS == 'T'

symIsRequired :: LibSymbol -> Bool
symIsRequired lS = symType lS == 'U'

symIsInteresting :: LibSymbol -> Bool
-- symIsInteresting lS = (symIsProvided lS) || (symIsRequired lS)
symIsInteresting = (||) <$> symIsProvided <*> symIsRequired

-- | Read a binary library and return a list of its symbols
readLibrarySymbols :: FilePath -> IO [LibSymbol]
readLibrarySymbols fileName = do
    out <- runNm fileName
    return $ parsePosixNmOutput out

runNm :: FilePath -> IO String
runNm fileName =
    readProcess "nm" ["--dynamic", "--format=posix", fileName] emptyStdin
  where
    emptyStdin = ""

-- ====== Parsing ======

parsePosixNmLine :: String -> LibSymbol
parsePosixNmLine l =
    LibSymbol { symType = t, symName = n }
  where
    (n, rest) = span (/= ' ') l
    t = rest !! 1

-- posix format: name space type [space other-info]
parsePosixNmOutput :: String -> [LibSymbol]
parsePosixNmOutput s = map parsePosixNmLine $ lines s

