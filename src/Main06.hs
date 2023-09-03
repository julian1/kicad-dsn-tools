
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


-- import Data.Text as T -- append, concat
import Data.Text.IO as T(readFile)



import Data.Attoparsec.Text ( {-Number(I, D),-} parseOnly)

import Data.Either(either)


---------------
import Lib
import DRCParser(drcParser )









-- don't care about unconnected
matchUnconnected :: DRCError -> Bool
matchUnconnected
  DRCError { _name = "unconnected_items" , _explanation , _features  } = True
matchUnconnected _  = False



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

  -- args:   directory   transform args

  args <- getArgs                  -- IO [String]
  mapM P.putStrLn args

  -- let dir  = P.head args
  let fileName : args_ = args

  -- should take a full file argument. not a directory.
  drc <- T.readFile fileName  -- $ dir ++ "/DRC.rpt"
  -- T.putStrLn drc;


  let drcParseResult = parseOnly drcParser drc

  let Right drcExpr = drcParseResult


  -- how do we combine these?? function...
  -- cleaner way to combine
  
  -- let (ignore, bad) = (L.partition (\x -> matchUnconnected x || clearanceWithCircle x || matchStarPad x) drcExpr)
  let (ignore, bad) = (L.partition (\x -> clearanceWithCircle x || matchStarPad x) drcExpr)



  mapM_ ( P.putStrLn .  show ) bad


  return ()







    -- convert to a set for easy lookup
  -- let sUnconnected  = S.fromList lunconnected
  -- T.putStrLn lunconnected






{-
  P.putStrLn  $ show $   (layerTransform  (Symbol "whoot")  )

--   P.putStrLn  $ show $   (layerTransform  3)
--        "4" -> trans8layer
--        _ -> error "whoot"

  {-
  Prelude> foldl (\x xs -> xs : x) []  [1,2,3]
  [3,2,1]

    -- id :: a -> a
    -- Prelude> (id . id) 3
    3


  -}



  -- TODO we need to chain the Either destructuring
  -- should be an easier way to chain this, so that left produces an errro
  either (\_ -> do
      T.putStrLn $ "drc file not valid"
    )
    ( \drcExpr -> do

            either (\_ -> do
                T.putStrLn $ "dsn file not valid"
              )
              (\dsnExpr -> do

                  let outName  = (dir ++ "/out.dsn")
                  P.putStrLn $ "writing to "  ++ outName

                  -- we want the file handling to happen at top level.
                  withFile  (dir ++ "/out.dsn") WriteMode  (\h -> do

                      doStuff h drcExpr dsnExpr
                    )

              ) dsnParseResult

    ) drcParseResult

-}




