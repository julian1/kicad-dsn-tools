
{-
  Use '--' to distinguish cabal and program args

  cabal  run main05 -- /home/me/devel/kicad6/projects/dmm05/specctra231-6-layer/ drc 65    -y | less

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


import Data.Set as S

import Data.Attoparsec.Text ( {-Number(I, D),-} parseOnly)

import Data.Either(either)


-- import Text.RawString.QQ
---------------
import Lib
import DRCParser(drcParser )

import ExprParser(exprParser)
import ExprPrint(exprPrint)






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
        not (
          S.member ( Pad_  pinNum  netClass designator  "" ) sUnconnected    -- is SMD connected.
          ||
          S.member ( PadTH_ pinNum  netClass designator  ) sUnconnected    -- or is TH pad connected
        )

{-
  -- this logic doesn't work. because it's inverted.

  -- having an AND turns it off

  -- this logic is hard.
  - we need to print the count of excluded pins.
-}


transformAddPinsIgnore :: S.Set PCBFeature -> Expr -> Expr
transformAddPinsIgnore sUnconnected expr =
  {-
    -- add additional field 'pins_ignore' but called 'off ' in order to avoid having to modify the freerouting flexer/scanner.

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

    List whoot@[
        Symbol "net",
        Symbol netClass ,
        (List (Symbol "pins" : pins))
      ]
      -> List $ whoot ++ [ List ( Symbol "off" : pins_ignore ) ]
        where
          pins_ignore = filterPins netClass sUnconnected pins


    List whoot@[
        Symbol "net",
        StringLit netClass ,
        (List (Symbol "pins": pins))
        ]
        -> List $ whoot ++ [ List ( Symbol "off" : pins_ignore ) ]
        where
          pins_ignore = filterPins netClass sUnconnected pins

    List xs
      -> List $  P.map (transformAddPinsIgnore sUnconnected) xs

    _ -> expr









-- create a transform, using the unconnected.

transUnconnected :: [ DRCError ] -> Expr -> Expr
transUnconnected drcExpr dsnExpr =

  let
    -- convert the drcExpression to the set of unconnected features, for easy lookup.
    -- better to change matchUnconnected name. to getUnconnected. or matchUnconnected
    lunconnected = mconcat $ P.map matchUnconnected drcExpr

    -- convert to a set for easy lookup
    sUnconnected  = S.fromList lunconnected

  in
    ( transformAddPinsIgnore   sUnconnected ) $ dsnExpr




trans8layer ::  Expr -> Expr
trans8layer expr =
  -- add use_layer directives for eight layer
  {-
      (circuit
        (use_via Via[0-5]_800:400_um)
    -> add
      (circuit
        (use_via Via[0-5]_800:400_um)
        ( use_layer F.Cu))
  -}
  case expr of
    -- match a class node, and extract class name, and pass it through recursion on children
    List ( Symbol "class" : className : xs )
      -> List ( (Symbol "class " ) : className : (P.map (helper className) xs ))

    List xs
      -> List $ P.map trans8layer xs

    _ -> expr

  where
    helper className expr =
      case expr of

        -- NICE pattern matching!.
        List whoot@((Symbol "circuit") : xs  )

          -> case className of
                -- we could construct these with parsed strings, to ease syntax if we wanted.
                -- https://wiki.haskell.org/MultiCase

                Symbol s
                  | s == "analog"  || s == "analog2" || s == "analog3" || s == "hc" || s == "hc2"
                      -> List $ whoot ++  [ List [ Symbol "use_layer", Symbol "F.Cu", Symbol "B.Cu", Symbol "In1.Cu", Symbol "In2.Cu", Symbol "In3.Cu" ] ]

                  | s == "hv" -> List $ whoot ++ [ List [ Symbol "use_layer", Symbol "F.Cu", Symbol "In1.Cu" ] ]

                  | s == "digital"  -> List $ whoot ++ [ List [ Symbol "use_layer", Symbol "F.Cu", Symbol "B.Cu", Symbol "In5.Cu", Symbol "In6.Cu"  ] ]

                  -- same as digital - but include In4, as local copper fill (gnd).
                  | s == "power"  -> List $ whoot ++ [ List [ Symbol "use_layer", Symbol "F.Cu", Symbol "B.Cu", Symbol "In4.Cu", Symbol "In5.Cu", Symbol "In6.Cu"  ] ]


                -- append error for unknown netclass
                -- _ -> List $ whoot ++ [ Symbol "ERROR"   ]
                _ -> List $ whoot

        _ -> expr





trans6layer ::  Expr -> Expr
trans6layer expr =
  case expr of
    -- add use_layer directives for six layer
    -- remember - we just give hi layer cost - to stop routing on top/gnd/bottom layers.

    List ( Symbol "class" : className : xs )
      -> List ( (Symbol "class " ) : className : (P.map (helper className) xs ))

    List xs
      -> List $ P.map trans6layer xs    -- 6 layer. careful. not to call co-recursively!!

    _ -> expr

  where
    helper className expr =
      case expr of

        List whoot@((Symbol "circuit") : xs  )

          -> case className of

                Symbol s
                  | s == "analog"  || s == "analog2" || s == "hc" || s == "hc2" || s == "digital"
                      -> List $ whoot ++  [ List [ Symbol "use_layer", Symbol "F.Cu", Symbol "B.Cu", Symbol "In1.Cu", Symbol "In2.Cu", Symbol "In4.Cu" ] ]

                  -- hv constrained to top layers
                  | s == "hv" -> List $ whoot ++ [ List [ Symbol "use_layer", Symbol "F.Cu", Symbol "In1.Cu" ] ]

                  -- power or guard - any layer. we reduce routing using high routing cost parameter.
                  | s == "power" || s == "guard"
                      -> List $ whoot ++  [ List [ Symbol "use_layer", Symbol "F.Cu", Symbol "B.Cu", Symbol "In1.Cu", Symbol "In2.Cu", Symbol "In3.Cu", Symbol "In4.Cu" ] ]

                _ -> List $ whoot

        _ -> expr







trans65layer ::  Expr -> Expr
trans65layer expr =
  case expr of
    -- add use_layer directives for six layer
    -- remember - we just give hi layer cost - to stop routing on top/gnd/bottom layers.

    List ( Symbol "class" : className : xs )
      -> List ( (Symbol "class " ) : className : (P.map (helper className) xs ))

    List xs
      -> List $ P.map trans65layer xs    -- 6 layer. careful. not to call co-recursively!!

    _ -> expr

  where
    helper className expr =
      case expr of

        List whoot@((Symbol "circuit") : xs  )

          -> case className of

                Symbol s
                  | s == "analog"  || s == "analog2" || s == "hc" || s == "hc2"
                      -> List $ whoot ++  [ List [ Symbol "use_layer", Symbol "F.Cu", Symbol "B.Cu", Symbol "In1.Cu", Symbol "In2.Cu" ] ]

                  -- digital and analog are constrained to top and bottom. although both share In1 with 6 layer.
                  |  s == "digital"
                      -> List $ whoot ++  [ List [ Symbol "use_layer", Symbol "F.Cu", Symbol "B.Cu", Symbol "In2.Cu", Symbol "In4.Cu" ] ]

                  -- hv constrained to top layers
                  | s == "hv" -> List $ whoot ++ [ List [ Symbol "use_layer", Symbol "F.Cu", Symbol "In1.Cu" ] ]

                  -- power or guard - prefer bottom layers like digital - must have access to In3 for copper fill to gnd.
                  -- we reduce routing using high routing cost parameter.
                  -- No. use bottom layers.
                  | s == "power" || s == "guard"
                      -> List $ whoot ++  [ List [ Symbol "use_layer", Symbol "F.Cu", Symbol "B.Cu",  Symbol "In2.Cu", Symbol "In3.Cu", Symbol "In4.Cu" ] ]


                _ -> List $ whoot

        _ -> expr














main :: IO ()
main =  do

  -- args:   directory   transform args

  args <- getArgs                  -- IO [String]
  mapM P.putStrLn args

  -- let dir  = P.head args
  let dir : args_ = args

  drc <- T.readFile $ dir ++ "/DRC.rpt"
  --  T.putStrLn drc;

  dsn <- T.readFile $ dir ++ "/main.dsn"
  --  putStrLn dsn



  let dsnParseResult = parseOnly exprParser dsn
  let drcParseResult = parseOnly drcParser drc


  -- fold over the arguments and build a transform function

  let transform = P.foldl  f  id (args_)
        where
        f transform  arg =
          case arg of
            "id" -> id . transform

            "6"  -> trans6layer . transform
            "65" -> trans65layer . transform

            "8" -> trans8layer . transform

            "drc" ->
              case drcParseResult of
                Right drcExpr -> (transUnconnected  drcExpr) . transform


            -- ignrore arguments prefixed with '-'
            _ -> case P.head arg of
                  '-' -> transform
                  _   -> error "unrecognized transform arg"




  -- fold over arguments with a '-' prefix, to extract flags

  let flags = P.foldl  f False (args_)
        where
        f flags' arg =
          case arg of
            "-write" -> True

            -- ignore unrecognized flags
            _ -> flags'


  -- write to file or to stdout
  case flags of
      -- use stdout
      False ->
        case dsnParseResult of
            Right  dsnExpr ->
              exprPrint stdout 0 (transform dsnExpr)


      -- really write a file as output
      True ->  do

        let outName  = (dir ++ "/out.dsn")

        P.putStrLn $ "writing to "  ++ outName

        -- we want the file handling to happen at top level.
        withFile  outName WriteMode  (\h -> do

          case dsnParseResult of
            Right  dsnExpr ->
              exprPrint h 0 (transform dsnExpr)
          )




  return ()













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




{-
transformPruneEmptyNets ::  Expr -> Expr
transformPruneEmptyNets expr =
  {-
    -- transformPruneEmptyNets empty nets from network
    -- this is not useful. doesn't work. because the nets will dropped on kicad re-imiport
    eg.
    (net LP15V
    (pins )
    ->
  -}

  case expr of

    List ( Symbol "network" : nets )

      -> List (Symbol "network" : nets2 )
      where
        nets2 =  P.filter f nets
        -- f x = True
        f x = case x of
          List [Symbol "net",
            (Symbol netClass  ) ,
            (List ( Symbol "pins" : pins))] | pins == [] -> False

          List [(Symbol "net" ),
            (StringLit netClass) ,
            (List ( Symbol "pins" : pins))] | pins == [] -> False

          _ -> True

    List xs
      -> List $  P.map transformPruneEmptyNets xs

    _ -> expr

-}





{-

doStuff ::  Handle -> [ DRCError ] -> Expr -> IO ()
doStuff h drcExpr dsnExpr = do
  {-
      TODO, this really doesn't need to run in the IO monad.
      except for intermediate data/error reporting
      should just return the transformed expression.
  -}


  -- show the drc expressions
  -- mapM_ ( Prelude.putStrLn .  show ) drcExpr

  -- convert the drcExpression to the set of unconnected features, for easy lookup.
  -- better to change matchUnconnected name. to getUnconnected. or matchUnconnected
  let lunconnected = mconcat $ P.map matchUnconnected drcExpr

  P.putStrLn $ "unconnected items " ++ (P.show . P.length) lunconnected

  -- print the unconnected features
  -- mapM_ ( P.putStrLn . show ) lunconnected


  -- convert to a set for easy lookup
  let sUnconnected  = S.fromList lunconnected

  -- test lookup a feature
  -- let isMember = S.member  ( Pad_  "12" "/ice40-2-200/C-MISO" "U212" "" ) sUnconnected
  -- T.putStrLn $ "isMember " `T.append` (pack . show $ isMember)

  let trsfmExpr = (trans8layer  . transformAddPinsIgnore   sUnconnected ) $ dsnExpr


  exprPrint h 0 trsfmExpr

  T.hPutStrLn stdout "" -- newline

-}





{-
  =====================
  there's an issue - some things that need to be connected are marked off.

    J301-Pad2. is off in the dsn. but it's unconnected.

    But it's marked - in the DRC as unconnected...

    [unconnected_items]: Missing connection between items
    Local override; Severity: error
    @(270.8450 mm, 43.7100 mm): Through hole pad 2 [Net-(J301-Pad2)] of J301


  It is marked as unconnected in the drc list.
  Issue with quoting? perhaps

    PadTH_ "2" "Net-(J301-Pad2)" "J301"


  happens on the through-hole connectors. J. designators.

  we could simplify the board.

  DRC looks like this.  No way to tell if through-hole.
  SO.

  ( net "Net-(J301-Pad2)"
      ( pins J301-2 SW301-5))


  So membership test. must be for both.
-}


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
