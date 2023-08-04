{-# LANGUAGE OverloadedStrings #-}

module Lib where


import Data.Attoparsec.Text (Number)    -- Comes from where???

import Data.Text (Text, unpack)

import qualified Data.Map as Map

-- type Symbol = String

-- data type for expression

data Expr
    -- Constructors for "Floating-point expression"
    =
    -- DoubleLit Double
    -- | SignedLit Integer


    SingleQuote


    | Num Number 


    | List [ Expr ]

    | StringLit Text

    | Symbol Text
    
    | Amp Text Text 

    deriving (Eq, Show)





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
