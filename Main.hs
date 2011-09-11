module Main where

{-

Reads input from stdin; writes output to stdout.  Input should be bare Unicode
or Velthuis string, terminated by newline, no other leading or trailing
whitespace.  Send EOF to complete.  Output has one of the following forms:

  (ok <result>)

        result is symbol, delimited with sticks (to avoid mucking about with
        escaping quotes); convert string.

  (error <message>)

        message is string describing failure.

In either case, output followed by newline.
-}

import qualified System.Exit as Exit
import qualified System.Environment.UTF8 as Env
-- Env.getArgs returns argv[1], argv[2], ...
import System.IO

import qualified Text.Devanagari.Segments as S
import qualified Text.Devanagari.Unicode as U
import qualified Text.Devanagari.Velthuis as V

main :: IO ()
main =
  do args <- Env.getArgs
     case args of
       ["--to-velthuis"] -> mainLoop unicodeToVelthuis
       ["--to-unicode"] -> mainLoop velthuisToUnicode
       _ -> printUsage

mainLoop :: (String -> Maybe String) -> IO ()
mainLoop convert = doIfNotEof (
  do line <- getLine
     case convert line of
       Just result -> putStrLn ("(ok |" ++ result ++ "|)")
       Nothing -> putStrLn "(error \"invalid unicode\")"
     mainLoop convert
     )

doIfNotEof :: IO () -> IO ()
doIfNotEof action =
  do eof <- isEOF
     if eof then return () else action

unicodeToVelthuis :: String -> Maybe String
unicodeToVelthuis str =
  fmap V.fromSegments (U.toSegments str)

velthuisToUnicode :: String -> Maybe String
velthuisToUnicode str =
  fmap U.fromSegments (U.toSegments str)

printUsage :: IO ()
printUsage =
  do putStrLn "devtrans: convert between Velthuis & Unicode representations"
     putStrLn "Usage: devtrans { --to-velthuis | --to-unicode }"
