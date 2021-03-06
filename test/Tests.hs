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

module Tests(tests) where

import Data.List (unzip4)
import Test.HUnit

import Control.Monad.Trans.Except (Except)

import qualified Test.Utils as TU

import Text.Devanagari.Exception
import Text.Devanagari.Segments
import qualified Text.Devanagari.Unicode as U
import qualified Text.Devanagari.Velthuis as V

import qualified Text.Devanagari.Exception as E

-- XXX fix hiatus: add vowel prefix to velthuis lookup map rather than mucking
-- about with isVowel predicate.  That way, we only stick {} after a or aa, and
-- only without visarga, anusvara.

-- | Test case that asserts that the two arguments are equal and evaluating the
-- left side does not throw a Devanagari exception.
(!?=) :: (Eq a, Show a) => Except Error a -> a -> Assertion
exnlComp !?= expected = TU.assertNoExcept expected exnlComp

-- | Asserts that the evaluation of a form throws a 'BadUnicode' exception.
assertBadUnicode :: (Show a) => Except Error a -> Assertion
assertBadUnicode = TU.assertExcept' isBadUnicode
  where isBadUnicode :: Error -> Assertion
        isBadUnicode (BadUnicode _ _) = return ()
        isBadUnicode e                =
          assertFailure ("Expected BadUnicode exn; got " ++ show e)

-- | Asserts that the evaluation of a form throws a 'BadVelthuis' exception.
assertBadVelthuis :: (Show a) => Except Error a -> Assertion
assertBadVelthuis = TU.assertExcept' isBadVelthuis
  where isBadVelthuis :: Error -> Assertion
        isBadVelthuis (BadVelthuis _ _) = return ()
        isBadVelthuis e                 =
          assertFailure ("Expected BadVelthuis exn; got " ++ show e)

data TestSpec = TS { label :: String,
                     unicode :: String,
                     velthuis :: String,
                     segments :: [Segment] }

tests =
  let (u2s, s2u, v2s, s2v) = unzip4 (map makeTest testSpecs)
  in "Text.Devanagari tests" ~:
     ["Unicode to Segments" ~: u2s,
      "Segments to Unicode" ~: s2u,
      "Velthuis to Segments" ~: v2s,
      "Segments to Velthuis" ~: s2v]
     ++ [unicodeErrorTests]
     ++ [velthuisErrorTests]

makeTest :: TestSpec -> (Test, Test, Test, Test)
makeTest (TS { label = l, unicode = u, segments = s, velthuis = v }) =
  (l ~: (U.toSegments u !?= s),
   l ~: (U.fromSegments s ~?= u),
   l ~: (V.toSegments v !?= s),
   l ~: (V.fromSegments s ~?= v))

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
   TS { label = "medial vowel hiatus, first implicit",
        unicode = "दउत",
        velthuis = "da{}uta",
        segments = [D, a, U NoMod, T, a]
      },
   TS { label = "medial vowel hiatus, first explicit",
        unicode = "दिउत",
        velthuis = "diuta", -- XXX do we need {} here?
        segments = [D, I NoMod, U NoMod, T, a]
      },
   TS { label = "hiatus with anusvara on first vowel",
        unicode = "देंउत",
        velthuis = "de.muta",
        segments = [D, E Anusvara, U NoMod, T, a]
      },
   TS { label = "hiatus with anusvara on second vowel",
        unicode = "देउंत",
        velthuis = "deu.mta",
        segments = [D, E NoMod, U Anusvara, T, a]
      },
   TS { label = "initial hiatus",
        unicode = "अउत",
        velthuis = "a{}uta",
        segments = [a, U NoMod, T, a]
      },
   TS { label = "final hiatus",
        unicode = "तेउ",
        velthuis = "teu",       -- XXX do we need {}
        segments = [T, E NoMod, U NoMod]
      },
   TS { label = "initial triple hiatus",
        unicode = "अउइत",
        velthuis = "a{}uita",
        segments = [a, U NoMod, I NoMod, T, a]
      },
   TS { label = "medial triple hiatus",
        unicode = "दउइत",
        velthuis = "da{}uita",
        segments = [D, a, U NoMod, I NoMod, T, a]
      },
   TS { label = "final triple hiatus",
        unicode = "तउइ",
        velthuis = "ta{}ui",
        segments = [T, a, U NoMod, I NoMod]
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
      }]
  ++ initialVowelTests

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

unicodeErrorTests =
  "invalid unicode input" ~:
  ["modifier after virama" ~: assertBadUnicode (U.toSegments "पत्ः"),
   "initial vowel after virama" ~: assertBadUnicode (U.toSegments "पत्ऋ"),
   "consecutive medial vowels" ~: assertBadUnicode (U.toSegments "पाीत्"),
   "medial vowel after modifier" ~: assertBadUnicode (U.toSegments "पःीत्"),
   "virama on initial vowel" ~: assertBadUnicode (U.toSegments "ए्क"),
   "virama on medial vowel" ~: assertBadUnicode (U.toSegments "पि्त")]

velthuisErrorTests =
  "invalid velthuis input" ~:
  ["modifier after medial consonant" ~:
   assertBadVelthuis (V.toSegments "pat.ha"),

   "modifier after final consonant" ~:
   assertBadVelthuis (V.toSegments "pat.h")]
