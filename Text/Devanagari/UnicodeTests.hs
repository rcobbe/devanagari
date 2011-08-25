module Text.Devanagari.UnicodeTests(tests) where

import Test.HUnit
import qualified Text.Devanagari.Phonemic as P
import qualified Text.Devanagari.Unicode as U

-- XXX include final visarga, medial visarga, medial anusvara, vowel hiatus
-- Samyoga at the end of a word?  Is that possible?

-- label, Unicode, phonemes
testSpecs :: [(String, String, [P.Phoneme])]
testSpecs =
  [("CV*, no implicit vowels", "कूपेषु", [P.K, P.UU, P.P, P.E, P.RetS, P.U]),
   ("CV*C, no implicit vowels", "कूपात्", [P.K, P.UU, P.P, P.AA, P.T]),
   ("CV*C, with implicit vowel", "वनम्", [P.V, P.A, P.N, P.A, P.M]),
   ("with samyoga", "कूपस्य", [P.K, P.UU, P.P, P.A, P.S, P.Y, P.A]),
   ("initial vowel with samyoga", "अश्वेषु",
    [P.A, P.PalS, P.V, P.E, P.RetS, P.U])]

tests =
  (TestLabel "Text.Devanagari.Unicode tests"
   (TestList
    ["Unicode to Phonemic" ~:
     TestList (map mkUtoPTest testSpecs),
     "Phonemic to Unicode" ~:
     TestList (map mkPtoUTest testSpecs)]))

mkUtoPTest :: (String, String, [P.Phoneme]) -> Test
mkUtoPTest (label, unicode, phonemes) =
  label ~: (U.toPhonemic unicode ~?= Just (phonemes, ""))

mkPtoUTest :: (String, String, [P.Phoneme]) -> Test
mkPtoUTest (label, unicode, phonemes) =
  label ~: (U.fromPhonemic phonemes ~?= Just unicode)
