-- Copyright 2012-2017 Richard Cobbe
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--   http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

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

import qualified System.Environment as Env
-- Env.getArgs returns argv[1], argv[2], ...
import System.IO
import System.Exit

import Control.Monad.Trans.Except (Except, runExcept)

import Text.Devanagari.Exception
import Text.Devanagari.Segments (Segment)
import qualified Text.Devanagari.Unicode as Unicode
import qualified Text.Devanagari.Velthuis as Velthuis

main :: IO ()
main =
  do args <- Env.getArgs
     case args of
       ["--to-velthuis"] ->
         mainLoop Unicode.toSegments Velthuis.fromSegments
       ["--to-unicode"] ->
         mainLoop Velthuis.toSegments Unicode.fromSegments
       _ -> printUsage

-- | The main loop of the conversion program.  Repeatedly reads lines from
-- stdin, applies the given conversion function, and writes output as
-- specified above, until detecting EOF on stdin.
mainLoop :: (String -> Except Error [Segment])
            -> ([Segment] -> String)
            -> IO ()
mainLoop toSegments fromSegments =
  repeatUntilEof
    (do line <- getLine
        Main.print (eval toSegments fromSegments line))

data Result = OK String
            | Error String

-- | Evaluate a single line; returns an IO action that either
eval :: (String -> Except Error [Segment])
         -> ([Segment] -> String)
         -> String
         -> Result
eval toSegments fromSegments line =
  case runExcept (toSegments line >>= return . fromSegments) of
    Left exn  -> Error $ userMessage exn
    Right val -> OK val

print :: Result -> IO ()
print (OK output) = putStrLn ("OK " ++ output)
print (Error msg) = putStrLn ("ERROR " ++ msg)

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
     exitWith (ExitFailure (-1))
