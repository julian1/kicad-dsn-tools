

{-# LANGUAGE QuasiQuotes,  OverloadedStrings #-}

{-
  - usage
    -  parse and write output expr for kicad_sch, kicad_pcb files
    - takes single file argument of dsn file.
-}



module Main where



import Prelude as P
import Data.Attoparsec.Text(parseOnly )

import Data.Either
import Lib

-- import Text.RawString.QQ

--import Data.Text as T
import Data.Text.IO as T(readFile, putStrLn, hPutStrLn)


import Data.Text as T


import System.IO(stdout)
import System.Environment (getArgs)

----

import ExprParser(exprParser)
import ExprPrint(exprPrint)


{-
  the problem with using a map  rather than a fold, is that we cannot discard the node expression.

  the problem with a filter,  is that we dont just need accept/reject.  but need to transform.
      actually we dont

    instead of Maybe.   why not use an empty list.  if have nothing.

-}

trans8layer ::  Expr -> Maybe Expr
trans8layer expr =
  {-
      (text ...
    -> (whoot ...
  -}

  case expr of
    -- match a class node, and extract class name, and pass it through recursion on children

    -- node we want to remove
    List ( Symbol "text" : (StringLit s) : xs )
      -> if T.take 2 s == "##"
          then  Just expr
          else Nothing


    -- recursive case
    List xs
      -> Just $ List $ P.foldr f []  xs

    -- catch all.
    _ -> Just expr

    where
      -- recursion and filter
      f x xs =
        case trans8layer x of
          Just j -> j : xs
          Nothing ->  xs




{-
  this is more complicated... than would like

-}


{-

ghci> foldr (\x xs -> x : xs )  [] [1..5]
[1,2,3,4,5]


        ‘foldr’ (imported from Prelude)
ghci> foldr (:) [] [1..5]
[1,2,3,4,5]

-}
  -- let transform = P.foldl  f  id (args_)



main :: IO ()
main =  do

  args <- getArgs                  -- IO [String]

  -- output filename
  -- mapM P.putStrLn args

  let file = P.head args

  dsn <- T.readFile file


  {- eg. (or 1 1),    (+ 1 (+ 1 1 ))
      putStrLn "enter an expression!"
      hFlush stdout
      ls <- getLine
  -}

  let exprParseResult = parseOnly exprParser dsn

  either (\_ -> do
      T.putStrLn $ "not a valid expr"
    )
    ( \expr -> do


        let Just expr1   = trans8layer expr


        exprPrint stdout 0 expr1

        T.hPutStrLn stdout "" -- newline

    ) exprParseResult




