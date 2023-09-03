
{-
  Use '--' to distinguish cabal and program args

  cabal run Main06 ./data/DRC-232.rpt 


-}


{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

-- {-# LANGUAGE  RecordDotSyntax  #-}




import System.IO
import System.Environment (getArgs)
import Prelude as P

import Data.Either


import Data.Text as T -- append, concat
import Data.Text.IO as T


-- import Data.Set as S

import Data.Attoparsec.Text ( {-Number(I, D),-} parseOnly)

import Data.Either(either)


---------------
import Lib
import DRCParser(drcParser )







{-
  return pcb features in the drc error expressions that are unconnected
  also normalize pcb features, by removing layer information.
-}


matchUnconnected :: DRCError -> [ PCBFeature  ]
matchUnconnected DRCError { _name =   "unconnected_items" , _explanation , _features  } =

    -- destructure FeatureItem to Feature.
    P.map ( f . _feature  ) _features where

      f (Pad_  pad nc c l ) = Pad_  pad nc c ""   -- remove layer info, to support set membership query without knowing layer
      f (PadTH_ pad nc c  ) = PadTH_ pad nc c
      f (Geom_ pad nc   )   = Geom_ pad nc
      f (Track_ nc l len )  = Track_ nc l len
      f (Via_ nc l )        = Via_ nc l
      f (Zone_ nc layer)    = Zone_ nc layer

matchUnconnected _ = [ ]









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



    -- convert the drcExpression to the set of unconnected features, for easy lookup.
    -- better to change matchUnconnected name. to getUnconnected. or matchUnconnected
  -- let lunconnected = mconcat $ P.map matchUnconnected drcExpr

 
  mapM_ ( P.putStrLn .  show ) drcExpr


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




