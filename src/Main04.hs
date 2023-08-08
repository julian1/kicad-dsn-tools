
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

-- {-# LANGUAGE  RecordDotSyntax  #-}


{-

  EXTR - even if there are a few odd errors related to planes, that prevent 'autoroute',  one can still run 'postroute' .

  -- GAHHH. when we reimport in kicad, again. all the traces get removed.  HMMMMM.
  -- So we would need to edit them back onto the session file???
  -- or we need another strategy to hide the net from freerouting but allow pass-through.
  ---
  -- actually editing the nets back into the network, - may not work - because the actual copper features may get removed, not just the net dependency.
  -- possible need to move nets to a non-routable netclass. if cannot otherwise disable.
  -- OR. rename them. eg.  CR_RESET to CR_RESET-NOROUTE and then rename back again.
  -- is it not possible to set an attribute?
  ----------

  EXTR.   .ses has 'network_out' rather than 'network'.  AND it doesn't include netclass information - which is good..
          SO. move nets to a non-routable netclass. and then the output will keep the net, but the change in netclass will not confuse kicad.

          alternatively maybe there is an attribute that could be set per net.


  -----
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

      -- recurse on child items
      T.putStr "("
      mapM_ (printExpr (level + 1)) xs
      T.putStr ")"

    Symbol s -> do
      T.putStr s

    Num s -> do
      -- T.putStr "{"
      T.putStr s
      -- T.putStr "}"

    SpecialIndex s -> do
      T.putStr s

    SingleQuote -> do
      T.putStr "\""

    StringLit s -> do
      T.putStr "\""
      T.putStr s
      T.putStr "\""





{-
  given drc error for unconnected items, return a tupple for easy lookup.
  not sure if really need this.  could just construct the feature at search to look it up.
  and preserve more of the typing.
  this does more than normalize - it pattern matches unconnected features.
-}


matchUnconnected :: DRCError -> [ PCBFeature  ]
matchUnconnected DRCError { _name =   "unconnected_items" , _explanation , _features  } =

    -- destructure FeatureItem to Feature.
    P.map ( f . _feature  ) _features where

      f (Pad_  pad nc c l ) = Pad_  pad nc c ""   -- remove layer, to support membership query without knowing layer
      f (PadTH_ pad nc c  ) = PadTH_ pad nc c
      f (Geom_ pad nc   )   = Geom_ pad nc
      f (Track_ nc l len )  = Track_ nc l len
      f (Via_ nc l )        = Via_ nc l

matchUnconnected _ = [ ]





getPinDesignator :: Text -> (Text, Text)
getPinDesignator pin = (pinNum, designator)
  where
  {- component should follow the form 'designator-pinNum'
    but should support the case if designator is itself hypenated.
  -}

  -- split on '-' separator
  parts = T.split (=='-') pin
  -- reverse to extract the last (first) element (eg. pinNum)
  (pinNum : xs) = P.reverse parts
  -- re-reverse and concatenate for designator
  designator = T.concat . P.reverse $ xs




filterPins :: Text -> S.Set PCBFeature -> [ Expr ]  -> [ Expr ]
filterPins netClass sUnconnected pins =

  P.filter f pins
    where
      f (Symbol pin ) =
        let
          (pinNum, designator) = getPinDesignator pin
        in
        S.member ( Pad_  pinNum  netClass designator  "" ) sUnconnected





transformExpr :: S.Set PCBFeature -> Expr -> Expr
transformExpr sUnconnected expr =
  {-
    -- transformPruneUnconnectedPins
    -  component pins from net if they do not appear in the drc unconnected
    eg.
    (net LP15V
    (pins U703-13 U414-13 U505-7 D404-3 U504-14 U301-3 U707-13 U1006-13 U907-7 U1003-13
    ->
    (net LP15V
    (pins U703-13 U414-13 ))
  -}
  case expr of
    -- the only differene between these matches - is the netClass which may be expressed as either a string literal or symbol

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

    List xs ->
      -- recurse into child nodes
      List $  P.map (transformExpr sUnconnected) xs


    _ -> expr




transformExpr2 ::  Expr -> Expr
transformExpr2 expr =
  {-
    -- transformPruneEmptyNets empty nets from network
    -- have to match at the network level in order
    eg.
    (net LP15V
    (pins )
    ->
  -}

  case expr of

    List ( (Symbol "network" ) : nets )  ->

      List ((Symbol "network" ) : nets2 )

      where
        nets2 =  P.filter f nets
        -- f x = True
        f x = case x of
          List [(Symbol "net" ),
            (Symbol netClass  ) ,
            (List ( (Symbol "pins") : pins))] | pins == [] -> False

          List [(Symbol "net" ),
            (StringLit netClass) ,
            (List ( (Symbol "pins") : pins))] | pins == [] -> False

          _ -> True


    List xs ->
      -- recurse into child nodes
      List $  P.map transformExpr2 xs


    _ -> expr












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

  let trsfmExpr = transformExpr2 . transformExpr sUnconnected $ dsnExpr

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



