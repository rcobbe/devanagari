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

import Control.Monad.Error

import Text.Devanagari.Exception
import Text.Devanagari.Segments (Segment)
import qualified Text.Devanagari.Unicode as Unicode
import qualified Text.Devanagari.Velthuis as Velthuis

main :: IO ()
main =
  do args <- Env.getArgs
     case args of
       ["--to-velthuis"] ->
         mainLoop Velthuis.fromSegments Unicode.toSegments
       ["--to-unicode"] ->
         mainLoop Unicode.fromSegments Velthuis.toSegments
       _ -> printUsage

-- | The main loop of the conversion program.  Repeatedly reads lines from
-- stdin, applies the given conversion function, and writes output as
-- specified above, until detecting EOF on stdin.
mainLoop :: (String -> Exceptional [Segment])
            -> [Segment] -> String
            -> IO ()
mainLoop toSegments fromSegments = repeatUntilEof (
  do line <- getLine
     (do segments <- toSegments getLine
         return (putStrLn ("OK " ++ (fromSegments segments))))
       `catchError`
       handler
  )
  where handler :: Error -> IO ()
        handler e =
          putStrLn ("ERROR " ++ msg e)

-- | Repeatedly executes the given action until it detects EOF on stdin.
repeatUntilEof :: IO () -> IO ()
repeatUntilEof action =
  do eof <- isEOF
     if eof then return () else action >> repeatUntilEof action

-- | Prints usage description to stdout.
printUsage :: IO ()
printUsage =
  -- XXX can we query for executable name instead of hardcoding?
  do putStrLn "devtrans: convert between Velthuis & Unicode representations"
     putStrLn "Usage: devtrans { --to-velthuis | --to-unicode }"
