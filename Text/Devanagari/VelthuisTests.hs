module Text.Devanagari.VelthuisTests(tests) where

import Test.HUnit
import qualified Text.Devanagari.Phonemic as P
import qualified Text.Devanagari.Velthuis as V

-- label, velthuis, phonemes
testSpecs :: [(String, String, [P.Phoneme])]
testSpecs =
  [("CV*, no implicit vowels", "kuupe.su",
    [P.K, P.UU P.NoMod, P.P, P.E P.NoMod, P.RetS, P.U P.NoMod]),
   ("CV*C, no implicit vowels", "kuupaat",
    [P.K, P.UU P.NoMod, P.P, P.AA P.NoMod, P.T]),
   ("CV*C, with implicit vowel", "vanam",
    [P.V, P.A P.NoMod, P.N, P.A P.NoMod, P.M]),
   ("with samyoga", "kuupasya",
    [P.K, P.UU P.NoMod, P.P, P.A P.NoMod, P.S, P.Y, P.A P.NoMod]),
   ("initial vowel with samyoga", "a\"sve.su",
    [P.A P.NoMod, P.PalS, P.V, P.E P.NoMod, P.RetS, P.U P.NoMod]),
   ("medial & final visargas", "du.hkhayo.h",
    [P.D, P.U P.Visarga, P.Kh, P.A P.NoMod, P.Y, P.O P.Visarga]),
   ("medial anusvara after a", "sa.msk.rt",
    [P.S, P.A P.Anusvara, P.S, P.K, P.VocR P.NoMod, P.T]),
   ("medial anusvara after written vowel", "si.mha",
    [P.S, P.I P.Anusvara, P.H, P.A P.NoMod]),
   ("vowel hiatus", "da{}uta",
    [P.D, P.A P.NoMod, P.U P.NoMod, P.T, P.A P.NoMod]),
   ("final samyoga", "rak.s",
    [P.R, P.A P.NoMod, P.K, P.RetS])
   ]

tests = (
  TestLabel "Text.Devanagari.Velthuis tests" (
     TestList [
        "Velthuis to Phonemic" ~:
        TestList (map mkVtoPTest testSpecs),
        "Phonemic to Velthuis" ~:
        TestList (map mkPtoVTest testSpecs)]))

mkVtoPTest :: (String, String, [P.Phoneme]) -> Test
mkVtoPTest (label, velthuis, phonemes) =
  label ~: (V.toPhonemic velthuis ~?= Just phonemes)

mkPtoVTest :: (String, String, [P.Phoneme]) -> Test
mkPtoVTest (label, velthuis, phonemes) =
  label ~: (V.fromPhonemic phonemes ~?= velthuis)
