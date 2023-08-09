
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}


import Prelude as P
import Data.Either

import Data.Text as T -- append, concat
import Data.Text.IO as T


import System.Environment (getArgs)
import Data.Attoparsec.Text ( parseOnly)

-- import Text.RawString.QQ
---------------
import Lib
import DRCParser(drcParser )


main :: IO ()
main =  do

  args <- getArgs                  -- IO [String]
  mapM P.putStrLn args

  let file = P.head args


  content <- T.readFile file

  let exprParseResult = parseOnly drcParser content



  either (\_ -> do
      T.putStrLn $ "not a valid drc expr"
    )
    ( \expr -> do

        mapM ( P.putStrLn .  show ) expr

        P.putStrLn "" -- newline

    ) exprParseResult



