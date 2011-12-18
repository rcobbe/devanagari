module Main where

{-

Reads input from stdin; writes output to stdout.  Input should be bare Unicode
or Velthuis string, terminated by newline, no other leading or trailing
whitespace.  Send EOF to complete.  Output has one of the following forms:

  OK <result>

    OK is at the beginning of the line, followed by a single space, then one or
    more result characters, followed immediately by a newline.

  ERROR <message>

    ERROR is at the beginning of the line, followed by a single space, then a
    new-line terminated string describing the error condition.
-}

import qualified Control.Exception as Exception

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

-- | The main loop of the conversion program.  Repeatedly reads lines from
-- stdin, applies the given conversion function, and writes output as
-- specified above, until detecting EOF on stdin.
mainLoop :: (String -> Maybe String) -> IO ()
mainLoop convert = repeatUntilEof (
  do line <- getLine
     case convert line of
       Just result -> putStrLn ("(ok |" ++ result ++ "|)")
       Nothing -> putStrLn "(error \"invalid unicode\")"
  )

-- | Repeatedly executes the given action until it detects EOF on stdin.
repeatUntilEof :: IO () -> IO ()
repeatUntilEof action =
  do eof <- isEOF
     if eof then return () else action >> repeatUntilEof action

unicodeToVelthuis :: String -> Maybe String
unicodeToVelthuis str =
  fmap V.fromSegments (U.toSegments str)

velthuisToUnicode :: String -> Maybe String
velthuisToUnicode str =
  fmap U.fromSegments (U.toSegments str)

printUsage :: IO ()
printUsage =
  -- XXX can we query for executable name instead of hardcoding?
  do putStrLn "devtrans: convert between Velthuis & Unicode representations"
     putStrLn "Usage: devtrans { --to-velthuis | --to-unicode }"
