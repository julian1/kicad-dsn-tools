
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

-- {-# LANGUAGE  RecordDotSyntax  #-}


{-

  - seem to have unconnected tracks.  or other compoonents.
  that are being passed through.

  - alternatively - we may have gotten better results - because we completely surpressed non 'digital' and 'digital2' netclasses, when we
    edited the output by hand.
  - also could consider removing empty pin lists.
  -------
  - moving the netclass entirely into a non-routable class - suppresses freerouting finding problems. .
    - So - we could prune all netclasses,  that are not part of a DRC unconnected item..
      eg. not jut remove all pins.
  ----------------
  NO - remove the net from the network. so the process has two steps.
    - 1. prune component pins from the net, unless unconnected.
    - 2. prune nets from network, if there are no component pins that need to be routed.  ie. so that other features - like vias/tracks, are not exposed.
    - 3. remove nets from netclasses  - and stick in nonroutable netclass. maybe. (what we did with manual edit, and code changes).
  ---
    - can also remove everything not manually routed, and just reroute.

  EXTR.  instead of selectively printing to stdout..
      we need to transform the expression. , to allow having more than one edit actions.
      then use a generic output.

-}


import Prelude as P

import Data.Either


import Data.Text as T -- append, concat
import Data.Text.IO as T


import Data.Set as S

import Data.Attoparsec.Text ( {-Number(I, D),-} parseOnly)



-- import Text.RawString.QQ
---------------
import Lib
import DRCParser(drcParser )

import ExprParser(exprParser)

import Data.Either(either)



{-
  -- given drc error for unconnected items, return a tupple for easy lookup.
  -- not sure if really need this.  could just construct the feature at search to look it up.
  -- and preserve more of the typing.
-}

-- this does more than normalize - it pattern matches unconnected features.

matchUnconnected :: DRCError -> [ PCBFeature  ]
matchUnconnected DRCError { _name =   "unconnected_items" , _explanation , _features  } =
  -- | _name == "unconnected_items"  =

    -- destructure FeatureItem to Feature.
    P.map ( f . _feature  ) _features where

      f (Pad_  pad nc c l ) = Pad_  pad nc c "" -- remove layer, to allow set membership test
      f (PadTH_ pad nc c  ) = PadTH_ pad nc c
      f (Geom_ pad nc   )   = Geom_ pad nc
      f (Track_ nc l len )  = Track_ nc l len
      f (Via_ nc l )        = Via_ nc l

matchUnconnected _ = [ ]





getPinDesignator :: Text -> (Text, Text)
getPinDesignator pin = (pinNum, designator)
  where
  -- component should follow the form 'designator-pinNum'
  -- but handle the case if designator is itself hypenated.

  -- split on '-' separator
  parts = T.split (=='-') pin
  -- reverse to extract the last (first) element (eg. pinNum)
  (pinNum : xs) = P.reverse parts
  -- re-reverse and concatenate for designator
  designator = T.concat . P.reverse $ xs



printPins :: Text -> S.Set PCBFeature -> [ Expr ]  ->  IO ()
printPins netClass sUnconnected pins = do

  T.putStr "( pins "
  mapM_ ( \(Symbol pin ) -> do

        let (pinNum, designator) = getPinDesignator pin
        let isMember = S.member ( Pad_  pinNum  netClass designator  "" ) sUnconnected
        {-
        T.putStr $ "pinNum " `T.append` pinNum
        T.putStr $ "designator " `T.append` designator
        T.putStr "     "
        T.putStrLn $ "isMember " `T.append` (pack . show $ isMember)
        -}
        if(isMember) then
          do
            T.putStr pin
            T.putStr " "
        else
          return ()

    ) pins

  T.putStr ")"

  -- we have to split apart the componennt and the pin number according to the '-' character.
  -- then construct a Pad, or PadTH
  return ()








{-
    Actually needs an identity like transform - with function that returns the input.
    then match on the List element - and transform.

-}

printExpr ::  Int -> Expr  ->  IO ()
printExpr level dsnExpr = do

  -- this can be our recursive walk of the dsn function. that tests membership
  T.putStr " "

  case dsnExpr of

    List xs -> do
      -- do indentation
      T.putStrLn ""   -- new line.
      let pad = T.justifyRight (level * 2 ) ' ' T.empty -- pad.
      T.putStr pad

      -- recurse on the items.
      T.putStr "("
      mapM_ (printExpr (level + 1)) xs
      T.putStr ")"



    Symbol s -> do
      T.putStr s

    Num s -> do
      -- T.putStr "{"
      T.putStr s
      -- T.putStr "}"

    Amp s -> do
      T.putStr s

    SingleQuote -> do
      T.putStr "\""

    StringLit s -> do
      T.putStr "\""
      T.putStr s
      T.putStr "\""





filterPins :: Text -> S.Set PCBFeature -> [ Expr ]  ->  [ Expr ]
filterPins netClass sUnconnected pins =

  P.filter f pins
    where
      f (Symbol pin ) =
        let
          (pinNum, designator) = getPinDesignator pin
        in
        S.member ( Pad_  pinNum  netClass designator  "" ) sUnconnected







transformExpr :: S.Set PCBFeature -> Int -> Expr ->  Expr
transformExpr sUnconnected level dsnExpr =

  -- don't need level/depth here, but it *could* be useful on other transforms.
  case dsnExpr of

    {-
        eg.

        (net LP15V
        (pins U703-13 U414-13 U505-7 D404-3 U504-14 U301-3 U707-13 U1006-13 U907-7 U1003-13
        ->
        (net LP15V
        (pins U703-13 U414-13 ))
    -}

    -- the only differene between these two - is if the netClass is expressed as string or symbol

    List [(Symbol "net" ),
          (Symbol netClass {-|| StringLit netClass -} ) ,
          (List ( (Symbol "pins") : pins))]  -> 
          let 
            pins2 = filterPins netClass sUnconnected pins
          in
            List [(Symbol "net" ),
                  (Symbol netClass  ) ,
                  (List ( (Symbol "pins") : pins2 ))]  



    List [(Symbol "net"),
          (StringLit netClass ) ,
          (List ( (Symbol "pins"): pins ))]  -> 
          let 
            pins2 = filterPins netClass sUnconnected pins
          in
            List [(Symbol "net" ),
                  (Symbol netClass  ) ,
                  (List ( (Symbol "pins") : pins2 ))]  

    List xs -> do

      -- transform list elements
      List $  P.map (transformExpr sUnconnected (level + 1)) xs



    _ -> dsnExpr



doStuff ::  [ DRCError ] -> Expr -> IO ()
doStuff drcExpr dsnExpr = do


  -- show the drc expressions
  -- mapM_ ( Prelude.putStrLn .  show ) drcExpr

  -- convert the drcExpression to the set of unconnected features, for easy lookup.
  -- better to change matchUnconnected name. to getUnconnected. or matchUnconnected
  let lunconnected = mconcat $ P.map matchUnconnected drcExpr

  -- print the unconnected features
  -- mapM_ ( Prelude.putStrLn . show ) lunconnected

  -- convert to a set for easy lookup
  let sUnconnected  = S.fromList lunconnected

  -- see if we can lookup a feature
  -- let isMember = S.member  ( Pad_  "12" "/ice40-2-200/C-MISO" "U212" "" ) sUnconnected
  -- T.putStrLn $ "isMember " `T.append` (pack . show $ isMember)

  let trsfmExpr = transformExpr sUnconnected 0 dsnExpr

  printExpr 0 trsfmExpr


-- now we want a function that takes the expression and the est


main :: IO ()
main =  do


  drc <- T.readFile "data/DRC.rpt"
  --  T.putStrLn drc;

  dsn <- T.readFile "data/main.dsn"
  --  putStrLn dsn


  let dsnParseResult = parseOnly exprParser dsn
  let drcParseResult = parseOnly drcParser drc


  -- TODO we need to chain the Either destructuring
  -- should be an easier way to chain this, so that left produces an errro
  either (\_ -> do
      T.putStrLn $ "drc file is not a valid experssion or statemet"
    )
    ( \drcExpr -> do

            either (\_ -> do
                T.putStrLn $ "dsn file is not a valid experssion or statemet"
              )
              (\dsnExpr -> do

                  doStuff drcExpr dsnExpr
              ) dsnParseResult

    ) drcParseResult



  return ()











{-
normalize1 :: DRCError -> [ (Text, Text, Text ) ]
normalize1  DRCError { _name = "unconnected_items"  , _explanation , _features  } =
  -- | _name == "unconnected_items"  =

    -- destructure FeatureItem to Feature.
    Prelude.map ( f . _feature  ) _features where

      f (Pad_  pad nc c l ) = ( nc, c, pad )
      f (PadTH_ pad nc c  ) = ( nc, c, pad )
      f (Geom_ pad nc   )   = ( nc, "geom", pad )
      f (Track_ nc l len )  = ( nc, "track", "" )
      f (Via_ nc l )        = ( nc, "via", "")

normalize1  _  = [ ]
-}


{-
  if isLeft drcParseResult
    then do
      T.putStrLn $ "not a valid experssion or statemet"
    else return ()


  if isLeft dsnParseResult
    then do
      T.putStrLn $ "not a valid experssion or statemet"
      -- exit...
    else return ()


      let Right drcExpr = drcParseResult

 --     mapM_ ( Prelude.putStrLn .  show ) expr


      let xs = mconcat $ Prelude.map normalize1 expr
      -- mapM_ ( Prelude.putStrLn .  show ) xs


      -- T.putStrLn ""

      let xs2 = mconcat $ Prelude.map matchUnconnected expr
      -- mapM_ ( Prelude.putStrLn .  show ) xs2


      let s  = S.fromList xs2

      let exists = S.member  ( Pad_  "12" "/ice40-2-200/C-MISO" "U212" "" ) s

      T.putStrLn $ T.concat [ "exists",  (pack. show $ exists) ]

-}




{-

f :: DRCError -> [ PCBFeature ]

f  DRCError { _name   , _explanation , _features  }
  | _name == "unconnected_items"  =

    -- destructure FeatureItem to Feature.
    Prelude.map ( f . _feature  ) _features where

      -- normalize so we can look it up again.
      f (Pad_  pad nc c l )  = Pad_  pad nc c ""

      f (PadTH_ pad nc c  )  = PadTH_ pad nc c

    -- [ ( _name , "whoot2", "whoot3" )  ]

f  _  = [ ]
-}

{-
  EXTR.  rather than storing as a tripple.
  instead. should several different sets...
  eg. for vias, and
  ---
  AND. we can drop the layer. for Pad.
  ---
  Need to check the network/net again.
  -----

  - It is really only the pads. that are of interest.... and we can normalize
  - By stuffing it in directly... and making layer empty.
    -----
  change the function name to normalize.
  But need to return Maybe...

-}


{-
  --- pad num, netclass, component, layer
  Pad_  Text Text Text Text


  -- padnum, netclass, component
  | PadTH_  Text Text Text

  -- netclass, layer
  | Geom_  Text Text

  -- netclass, layer, length
  | Track_  Text Text Text

  -- netclass, layer
  | Via_ Text Text
-}


-- OK. this is where we have the issue with the index. being numerical.
-- and it is quite complicated. could be double. or Int. etc.

