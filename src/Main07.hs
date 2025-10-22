

{-# LANGUAGE QuasiQuotes,  OverloadedStrings #-}

{-
  strip comments from a kicad s-exp file.  ie. schematic
-}



module Main where



import Prelude as P
import Data.Attoparsec.Text(parseOnly )

import Data.Either
import Lib

-- import Text.RawString.QQ

import Data.Text.IO as T(readFile, putStrLn, hPutStrLn)
import Data.Text as T

import System.IO(stdout)
import System.Environment (getArgs)

----

import ExprParser(exprParser)
import ExprPrint(exprPrint)


{-
  the problem with map  instead of a fold, is that we cannot discard a child node expression.

  the problem with filter,  is that perhaps we want more than binary include/reject a child node.


  instead of Maybe.   why not use an empty list.  if have nothing.
  but it doesn't work well with the idea of cons. the head.

-}


consMaybe :: Maybe a ->  [ a ] -> [ a ]
consMaybe (Just x) xs  = x : xs
consMaybe Nothing xs  = xs



trans ::  Expr -> Maybe Expr
trans expr =

  case expr of
    -- match node we want to work with

    -- node we want to remove
    List ( Symbol "text" : (StringLit s) : xs )

      | T.take 2 s /= "##"
      -> Nothing


    -- handle recursion
    List xs
      -> Just $ List $ P.foldr f []  xs
      where
        f x xs = consMaybe (trans x) xs

    -- catch all.
    _ -> Just expr




main :: IO ()
main =  do

  args <- getArgs                  -- IO [String]

  -- output filename
  -- mapM P.putStrLn args

  let file = P.head args

  dsn <- T.readFile file


  let exprParseResult = parseOnly exprParser dsn

  either (\_ -> do
      T.putStrLn $ "not a valid expr"
    )
    ( \expr -> do


        let Just expr1   = trans expr


        exprPrint stdout 0 expr1

        T.hPutStrLn stdout "" -- newline

    ) exprParseResult





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


  {- eg. (or 1 1),    (+ 1 (+ 1 1 ))
      putStrLn "enter an expression!"
      hFlush stdout
      ls <- getLine
  -}

