{-# LANGUAGE OverloadedStrings #-}

module Lib where


-- Peerhaps move this structure into the actual parser.

import Data.Attoparsec.Text (Number)    -- Comes from where???
import Data.Text (Text, unpack)
import qualified Data.Map as Map

-- type Symbol = String

-- data type for expression

data Expr =

    SingleQuote

    | Num Number 

    | List [ Expr ]

    | StringLit Text

    | Symbol Text
    
    | Amp Text Text 

    | Foo_  Foo

    deriving (Eq, Show)






data Foo = Foo { 


  _position :: Text,

  _padNum :: Integer ,
  
  _netClass :: Text,

  _component :: Text,
  
  _layer :: Text


  -- fooID     :: Int, 
  -- fooName   :: String 
} deriving (Eq, Show)


-- data type for evaluation result

{-
data Val
    = BoolVal Bool
    | NumVal Double
    | CharVal Char
    | NilVal
    | ConsVal Val Val
    deriving (Eq, Show)

-- data type for statement

data Stmt

    -- Execute a list of statements from left to right
    = StmtList [Stmt]

    -- Evaluate an expression, assign the result value to a variable; create the variable if it doesn't exist
    | SymbolSet Symbol Expr

    -- Evaluate the expression, if result is true, execute the left statement, otherwise if it's false, execute the right statement. If the expression doesn't return a boolean, it's an error
    | If Expr Stmt Stmt

    -- Repeatedly evaluate the expression, if result is true then execute the statement and repeat. The expression must return a boolean
    | While Expr Stmt

    -- Skip out one level of "while" loop. It's an error if currently we are not in a loop
    | Skip

    deriving (Show, Eq)



-- A program is a single statement
type Prog = Stmt

-- A memory is a mapping from variable names to values
type Mem = Map.Map Symbol Val

-}
