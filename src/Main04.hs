
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

-- {-# LANGUAGE  RecordDotSyntax  #-}


{-


  export dsn from kicad
  run this to generate main.dsn
  copy to spectra file.  edit to remove the cabal stuff at the top.
  
  run modified freerouting.
  export ses from freerouting
  import  ses into kica.
  the damn thing worked.

  except freerouting looks like it might be a bit stalled.
    maybe because old track is wrong for pin. need to fix in kicad.

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
      -- handle indentation
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
      T.putStr s

    SpecialIndex s -> do
      T.putStr s

    SingleQuote -> do
      T.putStr "\""

    StringLit s -> do
      T.putStr "\""
      T.putStr s
      T.putStr "\""





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

matchUnconnected _ = [ ]





getPinDesignator :: Text -> (Text, Text)
getPinDesignator pin = (pinNum, designator)
  {-
    extract pin and designator from the specctra expr
    support the case if designator is itself hypenated.
  -}
  where
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
        -- S.member ( Pad_  pinNum  netClass designator  "" ) sUnconnected
        S.notMember ( Pad_  pinNum  netClass designator  "" ) sUnconnected    -- pins to ignore.





transformExpr :: S.Set PCBFeature -> Expr -> Expr
transformExpr sUnconnected expr =
  {-
    -- pruneUnconnectedPins
    -  component pins from net if they do not appear in the drc unconnected
    eg.
    (net LP15V
    (pins U703-13 U414-13 U505-7 D404-3 U504-14 U301-3 U707-13 U1006-13 U907-7 U1003-13
    ->
    (net LP15V
    (pins U703-13 U414-13 U505-7 D404-3 U504-14 U301-3 U707-13 U1006-13 U907-7 U1003-13
    (off U703-13 U414-13 ))
  -}
  case expr of
    -- the only differene between these matches - is the netClass which may be expressed as either a string literal or symbol

    -- we don't even need the netclass
    -- the predicate matches the unconnected.
    -- while we want the inverted set.


    List [(Symbol "net" ),
          (Symbol netClass {-|| StringLit netClass -} ) ,
          (List ( (Symbol "pins") : pins))]  ->
          let
            pins2 = filterPins netClass sUnconnected pins
          in
            List [(Symbol "net" ),
                  (Symbol netClass  ) ,
                  (List ( (Symbol "pins") : pins )),
                  (List ( (Symbol "off")  : pins2 ))
                ]


    List [(Symbol "net"),
          (StringLit netClass ) ,
          (List ( (Symbol "pins"): pins ))]  ->
          let
            pins2 = filterPins netClass sUnconnected pins
          in
            List [(Symbol "net" ),
                  (StringLit netClass  ) ,              -- StringLit.   BE VERY CAREFUL HERE, otherwise output will not be properly quoted.
                  (List ( (Symbol "pins") : pins )),
                  (List ( (Symbol "off")  : pins2 ))
                ]

    List xs ->
      -- recurse into child nodes
      List $  P.map (transformExpr sUnconnected) xs


    _ -> expr




transformExpr2 ::  Expr -> Expr
transformExpr2 expr =
  {-
    -- pruneEmptyNets empty nets from network
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
  --------
  EXTR.   a per-net exclusion would be better than per netclass.  but some pins of a net we want to route. and some pins we don't , because already routed..

  EXTR.  IN freerouting - in the code we changed, is the test per-net or per-pin?
          i think it may be per pin.
          So. we might be able to spit out a separate file - which was our original plan.

  - Freerouting has Item. class which is a 'net' .  which is associated

  - OK.  it is more complicated.  we wouldn't just have two netclasses - routable and non-routable..
        But we would split the nets.
        AGND routable. AGND not-routable.
        No. changing the name of the net - would mean Kicad reimport won't work.


    - the problem is that adding AGND with so many piis - exposes every single via. to being reworked by freerouting.

      1727             Net net = this.board.rules.nets.get(net_no);
      1728             if( ! net.get_class().get_is_routable() ) {
      1729               return false;
      1730             }


    - The test is per net.   we need just need to change the code to indicate if the net is routable.    and then communicate that.
       THIS IS GOOD.

  - OK.
        the pin -> net -> netclass.

      - issue is we don't have clear - component and pin.
      item_should_be_ignored analog3, A400-5, board.Pin@1f2d5a31

      Pin.java class derives from Item. and has the component.
      also Via.java

      Component component = board.components.get(this.get_component_no());

      int padstack_no = component.get_package().get_pin(pin_no).padstack_no;

    For Pin.java derives from Item.  So we should be able to get info.

    -- look at the parser for pin. it might have an attribute as to whether it needs to be routed.

  this has to have everything.

    -- EXTR. IT MAY BE EASIER - TO change FREEROUTING - so it can read a PIN attribute - rather than communicating with a separate file.
        --  rather than have a separate file.
        --  and then we don't have to even to pin number and component number matching.
        -- the attribute would just be there.

        --- MUST CHECK.

      parsing function is,
      Network.read_net_pins: String expected

      src/main/java/designformats/specctra/Network.java:    private static boolean read_net_pins(Scanner p_scanner, Collection<Net.Pin> p_pin_list)

      Also, for reading the net.
      read_net_scope(Scanner p_scanner, NetList p_net_list, RoutingBoard p_board

      issue is that the Net structure is different to the Item.
  -------------- -----------------


    EXTR.  - something easier to parse - would be a list of 'pins_ignore'.
    OR - a separate list of ignore_pins

    -- scanner  is jflex. but not called by maven. so have to use a different word.

      -- 'off'  or 'none'.

    - it's very hard to see the handling of Net.Pin to Board.Pin

      https://github.com/flypie/freeRouting/

      ---------

    where is Pin created?

    src/main/java/board/BasicBoard.java:        Pin new_pin = new Pin(p_component_no, p_pin_no, p_net_no_arr, p_clearance_class, 0, p_fixed_state, this);


    seems to be most obvious. in BasicBoard.java
     public Pin insert_pin(int p_component_no, int p_pin_no, int[] p_net_no_arr, int p_clearance_class, FixedState p_fixed_state)

    And it is used in Network... GOOD....

    src/main/java/board/BasicBoard.java:    public Pin insert_pin(int p_component_no, int p_pin_no, int[] p_net_no_arr, int p_clearance_class, FixedState p_fixed_state)
    src/main/java/designformats/specctra/Network.java:            routing_board.insert_pin(new_component.no, i, net_no_arr, clearance_class, fixed_state);

    called by  in Network.java
       private static void insert_component(ComponentPlacement.ComponentLocation p_location, String p_lib_key,


    we could try adding extra variable. - and then hooking it up for the is_routable test.
    then we only need to communicate the value in the spectra file.  perhaps with a global/ var.



-}



{-
    EXTR. just override the should ignore... method.
    then print from there.

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
