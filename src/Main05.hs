
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

-- {-# LANGUAGE  RecordDotSyntax  #-}


{-

  Just transform to add the layer constraints.


-}



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

import ExprParser(exprParser)
import ExprPrint(exprPrint)









transformAddLayerDirective ::  Expr -> Expr
transformAddLayerDirective expr =
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
      -> List $ P.map transformAddLayerDirective xs

    _ -> expr

  where

    helper className expr =
      case expr of

        -- NICE pattern matching!.
        List whoot@((Symbol "circuit") : xs  )

          -> case className of
                -- we could construct these with parsed strings, to ease syntax if we wanted.
                -- https://wiki.haskell.org/MultiCase

                Symbol s | s == "analog"  || s == "analog2"
                  -> List $ whoot ++ [ List [ Symbol "use_layer", Symbol "F.Cu", Symbol "In1.Cu", Symbol "In2.Cu"  ] ]

                Symbol "analog3" -> List $ whoot ++ [ List [ Symbol "use_layer", Symbol "F.Cu", Symbol "In1.Cu", Symbol "In2.Cu", Symbol "B.Cu"  ] ]

                Symbol "digital" -> List $ whoot ++ [ List [ Symbol "use_layer", Symbol "F.Cu", Symbol "In4.Cu", Symbol "B.Cu"  ] ]

                Symbol s | s == "hc" || s == "hc2"
                    -> List $ whoot ++ [ List [ Symbol "use_layer", Symbol "F.Cu", Symbol "In1.Cu", Symbol "In4.Cu", Symbol "B.Cu" ] ]

                Symbol "hv" -> List $ whoot ++ [ List [ Symbol "use_layer", Symbol "F.Cu", Symbol "In1.Cu" ] ]

                Symbol "power" -> List $ whoot ++ [ List [ Symbol "use_layer", Symbol "F.Cu", Symbol "In2.Cu", Symbol "In4.Cu",  Symbol "B.Cu" ] ]

                _ -> List whoot

        _ -> expr






doStuff ::  Handle -> Expr -> IO ()
doStuff h dsnExpr = do
  {-
      TODO, this really doesn't need to run in the IO monad.
      except for intermediate data/error reporting
      should just return the transformed expression.
  -}



  let trsfmExpr = (transformAddLayerDirective  ) $ dsnExpr

  exprPrint h 0 trsfmExpr

  T.hPutStrLn stdout "" -- newline




main :: IO ()
main =  do

  args <- getArgs                  -- IO [String]
  mapM P.putStrLn args

  let dir  = P.head args

  dsn <- T.readFile $ dir ++ "/main.dsn"
  --  putStrLn dsn

  let dsnParseResult = parseOnly exprParser dsn

  either (\_ -> do
      T.putStrLn $ "dsn file not valid"
    )
    (\dsnExpr -> do

        let outName  = (dir ++ "/out.dsn")
        P.putStrLn $ "writing to "  ++ outName

        -- we want the file handling to happen at top level.
        withFile  (dir ++ "/out.dsn") WriteMode  (\h -> do

            doStuff h dsnExpr
          )

    ) dsnParseResult

  return ()



