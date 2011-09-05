module Text.Devanagari.Tests(tests) where

import Test.HUnit
import Data.List (unzip4)
import qualified Text.Devanagari.Phonemic as P
import qualified Text.Devanagari.Unicode as U
import qualified Text.Devanagari.Velthuis as V

-- XXX test coverage additions:
--  * hiatus with visarga, anusvara on first, second vowels
--  * initial, medial hiatus, with first vowel /a/ and otherwise
-- XXX fix hiatus: add vowel prefix to velthuis lookup map rather than mucking
-- about with isVowel predicate.  That way, we only stick {} after a or aa, and
-- only without visarga, anusvara.

data TestSpec = TS { label :: String,
                     unicode :: String,
                     velthuis :: String,
                     phonemes :: [P.Phoneme] }

tests =
  let (utop, ptou, vtop, ptov) = unzip4 (map makeTest testSpecs)
  in TestLabel "Text.Devanagari tests"
     (TestList
      ["Unicode to Phonemic" ~: TestList utop,
       "Phonemic to Unicode" ~: TestList ptou,
       "Velthuis to Phonemic" ~: TestList vtop,
       "Phonemic to Velthuis" ~: TestList ptov])

-- produces U-to-P, P-to-U, V-to-P, P-to-V tests.
makeTest :: TestSpec -> (Test, Test, Test, Test)
makeTest (TS { label = l, unicode = u, velthuis = v, phonemes = p }) =
  (l ~: (U.toPhonemic u ~?= Just p),
   l ~: (U.fromPhonemic p ~?= u),
   l ~: (V.toPhonemic v ~?= Just p),
   l ~: (V.fromPhonemic p ~?= v))

-- label, Unicode, phonemes
testSpecs :: [TestSpec]
testSpecs =
  let a = P.A P.NoMod
  in
   [TS { label = "CV*, no implicit vowels",
         unicode = "कूपेषु",
         velthuis = "kuupe.su",
         phonemes = [P.K, P.UU P.NoMod, P.P, P.E P.NoMod, P.RetS, P.U P.NoMod]
       },
    TS { label = "CV*C, no implicit vowels",
         unicode = "कूपात्",
         velthuis = "kuupaat",
         phonemes = [P.K, P.UU P.NoMod, P.P, P.AA P.NoMod, P.T]
       },
    TS { label = "CV*C, with implicit vowel",
         unicode = "वनम्",
         velthuis = "vanam",
         phonemes = [P.V, P.A P.NoMod, P.N, P.A P.NoMod, P.M]
       },
    TS { label = "with samyoga",
         unicode = "कूपस्य",
         velthuis = "kuupasya",
         phonemes = [P.K, P.UU P.NoMod, P.P, P.A P.NoMod, P.S, P.Y, P.A P.NoMod]
       },
    TS { label = "initial vowel with samyoga",
         unicode = "अश्वेषु",
         velthuis = "a\"sve.su",
         phonemes = [P.A P.NoMod, P.PalS, P.V, P.E P.NoMod, P.RetS, P.U P.NoMod]
       },
    TS { label = "medial & final visargas",
         unicode = "दुःखयोः",
         velthuis = "du.hkhayo.h",
         phonemes = [P.D, P.U P.Visarga, P.Kh, P.A P.NoMod, P.Y, P.O P.Visarga]
       },
    TS { label = "visarga after a, initial, medial, and final",
         unicode = "अःकःपः",
         velthuis = "a.hka.hpa.h",
         phonemes = [P.A P.Visarga, P.K, P.A P.Visarga, P.P, P.A P.Visarga]
       },
    TS { label = "visarga after initial i, medial e, and final au",
         unicode = "इःपेःतौः",
         velthuis = "i.hpe.htau.h",
         phonemes = [P.I P.Visarga, P.P, P.E P.Visarga, P.T, P.AU P.Visarga]
       },
    TS { label = "medial anusvara after a",
         unicode = "संस्कृत्",
         velthuis = "sa.msk.rt",
         phonemes = [P.S, P.A P.Anusvara, P.S, P.K, P.VocR P.NoMod, P.T] },
    TS { label = "medial anusvara after written vowel",
         unicode = "सिंह",
         velthuis = "si.mha",
         phonemes = [P.S, P.I P.Anusvara, P.H, P.A P.NoMod]
       },
    TS { label = "vowel hiatus",
         unicode = "दउत",
         velthuis = "da{}uta",
         phonemes = [P.D, P.A P.NoMod, P.U P.NoMod, P.T, P.A P.NoMod]
       },
    TS { label = "final samyoga",
         unicode = "रक्ष्",
         velthuis = "rak.s",
         phonemes = [P.R, P.A P.NoMod, P.K, P.RetS]
       },
    TS { label = "guttural consonants",
         unicode = "कखगघङ",
         velthuis = "kakhagagha\"na",
         phonemes = [P.K, a, P.Kh, a, P.G, a, P.Gh, a, P.Ng, a]
       },
    TS { label = "palatal consonants",
         unicode = "चछजझञ",
         velthuis = "cachajajha~na",
         phonemes = [P.C, a, P.Ch, a, P.J, a, P.Jh, a, P.PalN, a]
       },
    TS { label = "cerebral consonants",
         unicode = "टठडढण",
         velthuis = ".ta.tha.da.dha.na",
         phonemes = [P.RetT, a, P.RetTh, a, P.RetD, a, P.RetDh, a, P.RetN, a]
       },
    TS { label = "dental consonants",
         unicode = "तथदधन",
         velthuis = "tathadadhana",
         phonemes = [P.T, a, P.Th, a, P.D, a, P.Dh, a, P.N, a]
       },
    TS { label = "labial consonants",
         unicode = "पफबभम",
         velthuis = "paphababhama",
         phonemes = [P.P, a, P.Ph, a, P.B, a, P.Bh, a, P.M, a]
       },
    TS { label = "semivowels",
         unicode = "यरलव",
         velthuis = "yaralava",
         phonemes = [P.Y, a, P.R, a, P.L, a, P.V, a]
       },
    TS { label = "sibilants and H",
         unicode = "शषसह",
         velthuis = "\"sa.sasaha",
         phonemes = [P.PalS, a, P.RetS, a, P.S, a, P.H, a]
       },
    TS { label = "medial vowels",
         unicode = "ततातितीतुतूतृतॄतॢतॣतेतैतोतौ",
         velthuis = "tataatitiitutuut.rt.Rt.lt.Ltetaitotau",
         phonemes = [P.T, P.A P.NoMod,
                     P.T, P.AA P.NoMod,
                     P.T, P.I P.NoMod,
                     P.T, P.II P.NoMod,
                     P.T, P.U P.NoMod,
                     P.T, P.UU P.NoMod,
                     P.T, P.VocR P.NoMod,
                     P.T, P.VocRR P.NoMod,
                     P.T, P.VocL P.NoMod,
                     P.T, P.VocLL P.NoMod,
                     P.T, P.E P.NoMod,
                     P.T, P.AI P.NoMod,
                     P.T, P.O P.NoMod,
                     P.T, P.AU P.NoMod]
       }
    ]
  ++ initialVowelTests

initialVowelTests =
  map makeInitialVowelTest
  [("a", "अ", P.A),
   ("aa", "आ", P.AA),
   ("i", "इ", P.I),
   ("ii", "ई", P.II),
   ("u", "उ", P.U),
   ("uu", "ऊ", P.UU),
   (".r", "ऋ", P.VocR),
   (".R", "ॠ", P.VocRR),
   (".l", "ऌ", P.VocL),
   (".L", "ॡ", P.VocLL),
   ("e", "ए", P.E),
   ("ai", "ऐ", P.AI),
   ("o", "ओ", P.O),
   ("au", "औ", P.AU)]

makeInitialVowelTest :: (String, String, P.VowelMod -> P.Phoneme) -> TestSpec
makeInitialVowelTest (v, u, p) =
  TS { label = "initial " ++ v,
       unicode = u ++ "क",
       velthuis = v ++ "ka",
       phonemes = [p P.NoMod, P.K, P.A P.NoMod] }
