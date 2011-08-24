module Text.Devanagari.UnicodeTests(tests) where

import Test.HUnit
import qualified Text.Devanagari.Phonemic as P
import qualified Text.Devanagari.Unicode as U

-- XXX include final visarga, medial visarga, medial anusvara

tests =
  (TestLabel "Text.Devanagari.Unicode tests"
   (TestList
    ["from unicode: CV*, no implicit vowels" ~:
     (U.toPhonemic "कूपेषु") ~?=
     Just ([P.K, P.UU, P.P, P.E, P.RetS, P.U], ""),
     "from unicode; CV*C, no implicit vowels" ~:
     (U.toPhonemic "कूपात्") ~?=
     Just ([P.K, P.UU, P.P, P.AA, P.T], ""),
     "from unicode; CV*C, with implicit vowel" ~:
     (U.toPhonemic "वनम्") ~?=
     Just ([P.V, P.A, P.N, P.A, P.M], ""),
     "from unicode; with samyoga" ~:
     (U.toPhonemic "कूपस्य") ~?=
     Just ([P.K, P.UU, P.P, P.A, P.S, P.Y, P.A], ""),
     "from unicode: initial vowel with samyoga" ~:
     (U.toPhonemic "अश्वेषु") ~?=
     Just ([P.A, P.PalS, P.V, P.E, P.RetS, P.U], ""),
     "to unicode: CV*, no implicit vowels" ~:
     (U.fromPhonemic [P.K, P.UU, P.P, P.E, P.RetS, P.U]) ~?=
     Just "कूपेषु",
     "to unicode: CV*C, no implicit vowels" ~:
     (U.fromPhonemic [P.K, P.UU, P.P, P.AA, P.T]) ~?=
     Just "कूपात्",
     "to unicode: CV*C with implicit vowel" ~:
     (U.fromPhonemic [P.V, P.A, P.N, P.A, P.M]) ~?=
     Just "वनम्",
     "to unicode: with samyoga" ~:
     (U.fromPhonemic [P.K, P.UU, P.P, P.A, P.S, P.Y, P.A]) ~?=
     Just "कूपस्य",
     "to unicode: initial vowel with samyoga" ~:
     (U.fromPhonemic [P.A, P.PalS, P.V, P.E, P.RetS, P.U]) ~?=
     Just "अश्वेषु"]))


