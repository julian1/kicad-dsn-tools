{-# LANGUAGE OverloadedStrings #-}

module Lib where


-- Peerhaps move this structure into the actual parser.

import Data.Attoparsec.Text (Number)    -- Comes from where???
import Data.Text (Text, unpack)
import qualified Data.Map as Map

-- type Symbol = String

-- data type for expression
-- change name Sexpr.

data Expr =

    SingleQuote

    | Num Number

    | List [ Expr ]

    | StringLit Text

    | Symbol Text

    | Amp Text Text

    -- | PCBFeatureItem_  PCBFeatureItem

    deriving (Eq, Show)



------------------------

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




-- change name to PCBFeatureItem, and the FeatureItem.  as a tuple...

data PCBFeature  =
  -- or Via.

  --- pad num, netclass, component, layer
  Pad_  Integer  Text Text Text

  -- netclass, layer, length
  | Track_  Text Text Text

  -- netclass, layer
  | Via_ Text Text

  deriving (Eq, Show)






data PCBFeatureItem = PCBFeatureItem {
  -- Change this to a tuple object ?

  _position :: Text,


  _feature :: PCBFeature



} deriving (Eq, Show)




-- change to tupple

data DRCError = DRCError {

  _name :: Text,

  _explanation :: Text,

  _item1 :: PCBFeatureItem,

  _item2 :: PCBFeatureItem

} deriving (Eq, Show)




