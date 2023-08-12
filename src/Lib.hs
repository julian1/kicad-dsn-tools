{-# LANGUAGE OverloadedStrings #-}

module Lib where


-- Peerhaps move this structure into the actual parser.

import Data.Attoparsec.Text (Number)    -- Comes from where???
import Data.Text (Text, unpack)
import qualified Data.Map as Map


-- data type for expression
-- change name Sexpr.

data Expr =

    -- eg. for  (string_quote ")
    SingleQuote

    -- treat numbers as text as long as can
    | Num Text

    | List [ Expr ]

    | StringLit Text

    | Symbol Text

    -- integer, with an amperand. eg. 123@456
    | SpecialIndex Text


    deriving (Eq, Show)



------------------------



-- change name to PCBFeatureItem, and the FeatureItem.  as a tuple...

data PCBFeature  =
  -- or Via.

  -- Think should put netclass first. because it is common.
  -- No. leave in the order that it's parsed.

  --- pad num, netclass, component, layer
  Pad_  Text Text Text Text

  -- padnum, netclass, component
  | PadTH_  Text Text Text

  -- netclass, layer
  | Geom_  Text Text

  -- netclass, layer, length
  | Track_  Text Text Text

  -- netclass layer
  | Zone_  Text Text

  -- netclass, layer
  | Via_ Text Text

  deriving (Eq, Show, Ord)






data PCBFeatureItem = PCBFeatureItem {

  -- Change this to a tuple?

  _position :: Text,

  _feature :: PCBFeature

} deriving (Eq, Show)




-- change to tupple

data DRCError = DRCError {

  _name :: Text,

  _explanation :: Text,

  -- change name _features to _features plural.
  _features :: [ PCBFeatureItem ]

} deriving (Eq, Show)



{-
    rather than imposing this structure.
    Might be easier to just have named data constructors, for each element.

    Item, Pad, Layer  etc.

    and let the DRCExpr be recursive.
    name, val.
    or
    name ( val )
    name [ ] then list.
    value probably
    ----
    - key value pairs can be pattern matched... OK. but not easily. because they have to be in order.
    - so i think using structure.
-}



