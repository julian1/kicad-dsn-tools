
{-
  Use '--' to distinguish cabal and program args

  cabal run Main06 ./data/DRC-232.rpt


-}


{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}



import System.IO
import System.Environment (getArgs)
import Prelude as P

import Data.Either
import Data.List as L(partition)


-- import Data.Text as T(putStrLn) -- append, concat
import Data.Text.IO as T(readFile, putStrLn)



import Data.Attoparsec.Text ( {-Number(I, D),-} parseOnly)

import Data.Either(either)


---------------
import Lib
import DRCParser(drcParser )








{-
-- don't care about unconnected
matchUnconnected :: DRCError -> Bool
matchUnconnected
  DRCError { _name = "unconnected_items" , _explanation , _features  } = True
matchUnconnected _  = False
-}


-- chage name clearace with circle or polygon geometry
clearanceWithCircle :: DRCError -> Bool
clearanceWithCircle
  DRCError { _name = "clearance" , _explanation , _features  }
    | P.any f _features
    = True
  where
    f PCBFeatureItem { _feature = Geom_ "Circle" _ }  = True    -- for star
    f PCBFeatureItem { _feature = Geom_ "Polygon" _ }  = True   -- for netties
    f _   = False

clearanceWithCircle _  = False





-- a pad of a star connector
matchStarPad :: DRCError -> Bool
matchStarPad
  DRCError { _name = "clearance" , _explanation , _features  }
      | P.any  f  _features
      = True
  where
    f PCBFeatureItem { _feature = Pad_ _ _ connector  _  }
          | connector == "J308" || connector == "J310" || connector == "J311" || connector == "J312"   = True

    f _  = False

matchStarPad _ = False






main :: IO ()
main =  do

  args <- getArgs                  -- IO [String]
  mapM P.putStrLn args

  let fileName : args_ = args

  -- should take a full file argument. not a directory.
  drc <- T.readFile fileName

  -- T.putStrLn drc;


  let drcParseResult = parseOnly drcParser drc

  let Right drcExpr = drcParseResult


  -- how do we combine these?? function...
  -- cleaner way to combine

  -- let (ignore, bad) = (L.partition (\x -> matchUnconnected x || clearanceWithCircle x || matchStarPad x) drcExpr)
  let (ignore, bad) = (L.partition (\x -> clearanceWithCircle x || matchStarPad x) drcExpr)



  mapM_ ( P.putStrLn .  show ) bad


  return ()






