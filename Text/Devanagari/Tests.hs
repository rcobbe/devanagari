module Text.Devanagari.Tests(tests) where

import Test.HUnit
import Data.List (unzip4)
import qualified Text.Devanagari.Segments as S
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
                     phonemes :: [S.Segment] }

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
  (l ~: (U.toSegments u ~?= Just p),
   l ~: (U.fromSegments p ~?= u),
   l ~: (V.toSegments v ~?= Just p),
   l ~: (V.fromSegments p ~?= v))

-- label, Unicode, phonemes
testSpecs :: [TestSpec]
testSpecs =
  let a = S.A S.NoMod
  in
   [TS { label = "CV*, no implicit vowels",
         unicode = "कूपेषु",
         velthuis = "kuupe.su",
         phonemes = [S.K, S.UU S.NoMod, S.P, S.E S.NoMod, S.RetS, S.U S.NoMod]
       },
    TS { label = "CV*C, no implicit vowels",
         unicode = "कूपात्",
         velthuis = "kuupaat",
         phonemes = [S.K, S.UU S.NoMod, S.P, S.AA S.NoMod, S.T]
       },
    TS { label = "CV*C, with implicit vowel",
         unicode = "वनम्",
         velthuis = "vanam",
         phonemes = [S.V, S.A S.NoMod, S.N, S.A S.NoMod, S.M]
       },
    TS { label = "with samyoga",
         unicode = "कूपस्य",
         velthuis = "kuupasya",
         phonemes = [S.K, S.UU S.NoMod, S.P, S.A S.NoMod, S.S, S.Y, S.A S.NoMod]
       },
    TS { label = "initial vowel with samyoga",
         unicode = "अश्वेषु",
         velthuis = "a\"sve.su",
         phonemes = [S.A S.NoMod, S.PalS, S.V, S.E S.NoMod, S.RetS, S.U S.NoMod]
       },
    TS { label = "medial & final visargas",
         unicode = "दुःखयोः",
         velthuis = "du.hkhayo.h",
         phonemes = [S.D, S.U S.Visarga, S.Kh, S.A S.NoMod, S.Y, S.O S.Visarga]
       },
    TS { label = "visarga after a, initial, medial, and final",
         unicode = "अःकःपः",
         velthuis = "a.hka.hpa.h",
         phonemes = [S.A S.Visarga, S.K, S.A S.Visarga, S.P, S.A S.Visarga]
       },
    TS { label = "visarga after initial i, medial e, and final au",
         unicode = "इःपेःतौः",
         velthuis = "i.hpe.htau.h",
         phonemes = [S.I S.Visarga, S.P, S.E S.Visarga, S.T, S.AU S.Visarga]
       },
    TS { label = "medial anusvara after a",
         unicode = "संस्कृत्",
         velthuis = "sa.msk.rt",
         phonemes = [S.S, S.A S.Anusvara, S.S, S.K, S.VocR S.NoMod, S.T] },
    TS { label = "medial anusvara after written vowel",
         unicode = "सिंह",
         velthuis = "si.mha",
         phonemes = [S.S, S.I S.Anusvara, S.H, S.A S.NoMod]
       },
    TS { label = "vowel hiatus",
         unicode = "दउत",
         velthuis = "da{}uta",
         phonemes = [S.D, S.A S.NoMod, S.U S.NoMod, S.T, S.A S.NoMod]
       },
    TS { label = "final samyoga",
         unicode = "रक्ष्",
         velthuis = "rak.s",
         phonemes = [S.R, S.A S.NoMod, S.K, S.RetS]
       },
    TS { label = "guttural consonants",
         unicode = "कखगघङ",
         velthuis = "kakhagagha\"na",
         phonemes = [S.K, a, S.Kh, a, S.G, a, S.Gh, a, S.Ng, a]
       },
    TS { label = "palatal consonants",
         unicode = "चछजझञ",
         velthuis = "cachajajha~na",
         phonemes = [S.C, a, S.Ch, a, S.J, a, S.Jh, a, S.PalN, a]
       },
    TS { label = "cerebral consonants",
         unicode = "टठडढण",
         velthuis = ".ta.tha.da.dha.na",
         phonemes = [S.RetT, a, S.RetTh, a, S.RetD, a, S.RetDh, a, S.RetN, a]
       },
    TS { label = "dental consonants",
         unicode = "तथदधन",
         velthuis = "tathadadhana",
         phonemes = [S.T, a, S.Th, a, S.D, a, S.Dh, a, S.N, a]
       },
    TS { label = "labial consonants",
         unicode = "पफबभम",
         velthuis = "paphababhama",
         phonemes = [S.P, a, S.Ph, a, S.B, a, S.Bh, a, S.M, a]
       },
    TS { label = "semivowels",
         unicode = "यरलव",
         velthuis = "yaralava",
         phonemes = [S.Y, a, S.R, a, S.L, a, S.V, a]
       },
    TS { label = "sibilants and H",
         unicode = "शषसह",
         velthuis = "\"sa.sasaha",
         phonemes = [S.PalS, a, S.RetS, a, S.S, a, S.H, a]
       },
    TS { label = "medial vowels",
         unicode = "ततातितीतुतूतृतॄतॢतॣतेतैतोतौ",
         velthuis = "tataatitiitutuut.rt.Rt.lt.Ltetaitotau",
         phonemes = [S.T, S.A S.NoMod,
                     S.T, S.AA S.NoMod,
                     S.T, S.I S.NoMod,
                     S.T, S.II S.NoMod,
                     S.T, S.U S.NoMod,
                     S.T, S.UU S.NoMod,
                     S.T, S.VocR S.NoMod,
                     S.T, S.VocRR S.NoMod,
                     S.T, S.VocL S.NoMod,
                     S.T, S.VocLL S.NoMod,
                     S.T, S.E S.NoMod,
                     S.T, S.AI S.NoMod,
                     S.T, S.O S.NoMod,
                     S.T, S.AU S.NoMod]
       }
    ]
  ++ initialVowelTests

initialVowelTests =
  map makeInitialVowelTest
  [("a", "अ", S.A),
   ("aa", "आ", S.AA),
   ("i", "इ", S.I),
   ("ii", "ई", S.II),
   ("u", "उ", S.U),
   ("uu", "ऊ", S.UU),
   (".r", "ऋ", S.VocR),
   (".R", "ॠ", S.VocRR),
   (".l", "ऌ", S.VocL),
   (".L", "ॡ", S.VocLL),
   ("e", "ए", S.E),
   ("ai", "ऐ", S.AI),
   ("o", "ओ", S.O),
   ("au", "औ", S.AU)]

makeInitialVowelTest :: (String, String, S.VowelMod -> S.Segment) -> TestSpec
makeInitialVowelTest (v, u, p) =
  TS { label = "initial " ++ v,
       unicode = u ++ "क",
       velthuis = v ++ "ka",
       phonemes = [p S.NoMod, S.K, S.A S.NoMod] }
