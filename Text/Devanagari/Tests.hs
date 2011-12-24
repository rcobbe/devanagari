{-# LANGUAGE ScopedTypeVariables #-}

module Text.Devanagari.Tests(tests) where

import Control.Exception (try)
import Data.List (unzip4)
import Test.HUnit

import Text.Devanagari.Exception
import Text.Devanagari.Segments
import qualified Text.Devanagari.Unicode as U
import qualified Text.Devanagari.Velthuis as V

import qualified Text.Devanagari.Exception as E

-- | Test case that asserts that the two arguments are equal and evaluating the
-- left side does not throw a Devanagari exception.
(!?=) :: (Eq a, Show a) => a -> a -> Assertion
actual !?= expected =
  do result <- try (return actual)
     case result of
       Left (exn :: E.Exception) ->
         assertFailure ("caught exception: " ++ show exn)
       Right val -> val @?= expected

-- | Asserts that the evaluation of a form terminates in a BadUnicode
-- exception.
assertBadUnicode :: (Show a) => a -> Assertion
assertBadUnicode form =
  do result <- try (return form)
     case result of
       Left (BadUnicode _ _) -> return ()
       {-
       Left exn ->
         assertFailure ("expected BadUnicode exception; caught " ++ show exn)
       -}
       Right val ->
         assertFailure
         ("expected BadUnicode exception; got result " ++ show val)

data TestSpec = TS { label :: String,
                     unicode :: String,
                     velthuis :: String,
                     segments :: [Segment] }

tests =
  -- let (u2s, s2u, v2s, s2v) = unzip4 (map makeTest testSpecs)
  let u2s = map makeTest testSpecs
  in "Text.Devanagari tests" ~:
     ["Unicode to Segments" ~: u2s]

makeTest :: TestSpec -> Test
makeTest (TS { label = l, unicode = u, segments = s }) =
  (l ~: (U.toSegments u !?= s))

a :: Segment
a = A NoMod

testSpecs :: [TestSpec]
testSpecs =
  [TS { label = "(CV)*, no implicit vowels",
        unicode = "कूपेषु",
        velthuis = "kuupe.su",
        segments = [K, UU NoMod, P, E NoMod, RetS, U NoMod]
      },
   TS { label = "CV*C, no implicit vowels",
        unicode = "कूपात्",
        velthuis = "kuupaat",
        segments = [K, UU NoMod, P, AA NoMod, T]
      },
   TS { label = "CV*C, with implicit vowel",
        unicode = "वनम्",
        velthuis = "vanam",
        segments = [V, a, N, a, M]
      },
   TS { label = "with samyoga",
        unicode = "कूपस्य",
        velthuis = "kuupasya",
        segments = [K, UU NoMod, P, a, S, Y, a]
      },
   TS { label = "initial vowel with samyoga",
        unicode = "अश्वेषु",
        velthuis = "a\"sve.su",
        segments = [a, PalS, V, E NoMod, RetS, U NoMod]
      },
   TS { label = "medial & final visargas",
        unicode = "दुःखयोः",
        velthuis = "du.hkhayo.h",
        segments = [D, U Visarga, Kh, a, Y, O Visarga]
      },
   TS { label = "visarga after a, initial, medial, and final",
        unicode = "अःकःपः",
        velthuis = "a.hka.hpa.h",
        segments = [A Visarga, K, A Visarga, P, A Visarga]
      },
   TS { label = "visarga after initial i, medial e, and final au",
        unicode = "इःपेःतौः",
        velthuis = "i.hpe.htau.h",
        segments = [I Visarga, P, E Visarga, T, AU Visarga]
      },
   TS { label = "medial anusvara after a",
        unicode = "संस्कृत्",
        velthuis = "sa.msk.rt",
        segments = [S, A Anusvara, S, K, VocR NoMod, T] },
   TS { label = "medial anusvara after written vowel",
        unicode = "सिंह",
        velthuis = "si.mha",
        segments = [S, I Anusvara, H, a]
      },
   TS { label = "vowel hiatus",
        unicode = "दउत",
        velthuis = "da{}uta",
        segments = [D, a, U NoMod, T, a]
      },
   TS { label = "single initial vowel",
        unicode = "ए",
        velthuis = "e",
        segments = [E NoMod]
      },
   TS { label = "single initial vowel with modifier",
        unicode = "एः",
        velthuis = "e.h",
        segments = [E Visarga]
      },
   TS { label = "final samyoga",
        unicode = "रक्ष्",
        velthuis = "rak.s",
        segments = [R, a, K, RetS]
      },
   TS { label = "guttural consonants",
        unicode = "कखगघङ",
        velthuis = "kakhagagha\"na",
        segments = [K, a, Kh, a, G, a, Gh, a, Ng, a]
      },
   TS { label = "palatal consonants",
        unicode = "चछजझञ",
        velthuis = "cachajajha~na",
        segments = [C, a, Ch, a, J, a, Jh, a, PalN, a]
      },
   TS { label = "cerebral consonants",
        unicode = "टठडढण",
        velthuis = ".ta.tha.da.dha.na",
        segments = [RetT, a, RetTh, a, RetD, a, RetDh, a, RetN, a]
      },
   TS { label = "dental consonants",
        unicode = "तथदधन",
        velthuis = "tathadadhana",
        segments = [T, a, Th, a, D, a, Dh, a, N, a]
      },
   TS { label = "labial consonants",
        unicode = "पफबभम",
        velthuis = "paphababhama",
        segments = [P, a, Ph, a, B, a, Bh, a, M, a]
      },
   TS { label = "semivowels",
        unicode = "यरलव",
        velthuis = "yaralava",
        segments = [Y, a, R, a, L, a, V, a]
      },
   TS { label = "sibilants and H",
        unicode = "शषसह",
        velthuis = "\"sa.sasaha",
        segments = [PalS, a, RetS, a, S, a, H, a]
      },
   TS { label = "medial vowels",
        unicode = "ततातितीतुतूतृतॄतॢतॣतेतैतोतौ",
        velthuis = "tataatitiitutuut.rt.Rt.lt.Ltetaitotau",
        segments = [T, a,
                    T, AA NoMod,
                    T, I NoMod,
                    T, II NoMod,
                    T, U NoMod,
                    T, UU NoMod,
                    T, VocR NoMod,
                    T, VocRR NoMod,
                    T, VocL NoMod,
                    T, VocLL NoMod,
                    T, E NoMod,
                    T, AI NoMod,
                    T, O NoMod,
                    T, AU NoMod]
      }
  ] ++ initialVowelTests

initialVowelTests =
  map makeInitialVowelTest
  [("a", "अ", A),
   ("aa", "आ", AA),
   ("i", "इ", I),
   ("ii", "ई", II),
   ("u", "उ", U),
   ("uu", "ऊ", UU),
   (".r", "ऋ", VocR),
   (".R", "ॠ", VocRR),
   (".l", "ऌ", VocL),
   (".L", "ॡ", VocLL),
   ("e", "ए", E),
   ("ai", "ऐ", AI),
   ("o", "ओ", O),
   ("au", "औ", AU)]

-- Velthuis, Unicode, vowel segment constructor
makeInitialVowelTest :: (String, String, VowelMod -> Segment) -> TestSpec
makeInitialVowelTest (v, u, s) =
  TS { label = "initial " ++ v,
       unicode = u ++ "क",
       velthuis = v ++ "ka",
       segments = [s NoMod, K, a] }
