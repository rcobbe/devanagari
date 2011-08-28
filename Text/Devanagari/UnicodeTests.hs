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
    [P.A, P.PalS, P.V, P.E, P.RetS, P.U]),
   ("medial & final visargas", "दुःखयोः",
    [P.D, P.U, P.Visarga, P.Kh, P.A, P.Y, P.O, P.Visarga]),
   ("medial anusvara after a", "संस्कृत्",
    [P.S, P.A, P.Anusvara, P.S, P.K, P.VocR, P.T]),
   ("medial anusvara after written vowel", "सिंह",
    [P.S, P.I, P.Anusvara, P.H, P.A]),
   ("vowel hiatus", "दउत", [P.D, P.A, P.U, P.T, P.A]),
   ("final samyoga", "रक्ष्", [P.R, P.A, P.K, P.RetS])
   ]

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
