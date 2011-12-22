-- | This module defines the translations between devanagari representations
-- based on Unicode and on 'Text.Devanagari.Segments.Segment'.

module Text.Devanagari.Unicode(toSegments, fromSegments) where

-- XXX can we get rid of the qualification in the haddock comment at top of
-- file?

-- XXX fix parser.   See below:

{-
devanagari:
  - anusvara, visarga on [a]
  - consonant cluster ending word
  - word consisting of single (initial) vowel

syllable rule seems to be
  optional initial vowel
  zero or more consonant-virama pairs
  optional (consonant w/o virama followed by (implicit) vowel with
    modifiers)

this should handle words that end with a consonant

tricky things to test:
  - word consisting only of a single vowel
  - vowel hiatus: double, triple; intial, medial, final
  - vowel modifiers:
    - according to TextEdit, anusvara, visarga must always follow vowel,
      initial or otherwise
    - initial, medial, final
    - in hiatus (first, middle, last)
    - on implicit short-a
    - after virama (should signal error)
  - initial vowel after consonant (should signal error)
-}

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Prim
import Text.Parsec.String

import Text.Devanagari.Segments

-- | Converts a Unicode string to a list of segments.  On error throws one of
-- the following:
-- *
toSegments :: String -> [Segment]
toSegments s =
  case parse unicode "" s of
    Left error -> undefined
    Right segments -> segments

fromSegments :: [Segment] -> String
fromSegments = undefined

unicode :: GenParser Char st [Segment]
unicode =
  (do v <- initialVowel


  do syllables <- manyTill
                  unicodeSyllable eof
     return $ concat syllables

unicodeSyllable :: GenParser char st [Segment]
unicodeSyllable =
  do initVowel <- option [] unicodeInitialVowel
     clusterOnset <- unicodeClusterConsonants
     syllableCoda <- unicodeSyllableCoda
     return concat [initVowel, clusterOnset, syllableCoda]

unicodeSyllableCoda :: GenParser char st [Segment]
unicodeSyllableCoda =
  do cnsnt <- unicodeBareConsonant
     -- optional because the word might end with a consonant
     vowel <- option (A NoMod) unicodeMedialVowel
     return [cnsnt, vowel]

unicodeInitialVowel :: GenParser Char st [Segment]
unicodeInitialVowel =
  do vowelCtor <- charTranslate [(initA, A),
                                 (initAA, AA),
                                 (initI, I),
                                 (initII, II),
                                 (initU, U),
                                 (initUU, UU),
                                 (initVocR, VocR),
                                 (initVocRR, VocRR),
                                 (initVocL, VocL),
                                 (initVocLL, VocLL),
                                 (initE, E),
                                 (initAI, AI),
                                 (initO, O),
                                 (initAU, AU)]
     mod <- option NoMod unicodeVowelModifier
     return [vowelCtor mod]

unicodeMedialVowel :: GenParser Char st [Segment]
unicodeMedialVowel =
  do vowelCtor <- charTranslate [(combAA, AA),
                                 (combI, I),
                                 (combII, II),
                                 (combU, U),
                                 (combUU, UU),
                                 (combVocR, VocR),
                                 (combVocRR, VocRR),
                                 (combVocL, VocL),
                                 (combVocLL, VocLL),
                                 (combE, E),
                                 (combAI, AI),
                                 (combO, O),
                                 (combAU, AU)]
     mod <- option NoMod unicodeVowelModifier
     return [vowelCtor mod]

unicodeVowelModifier :: GenParser Char st VowelMod
unicodeVowelModifier =
  charTranslate [(visarga, Visarga), (anusvara, Anusvara)]
  <|> return NoMod

unicodeClusterConsonants :: GenParser Char st [Segment]
unicodeClusterConsonants =
  many (do cnsnt <- unicodeBareConsonant
           char virama
           return cnsnt)

unicodeBareConsonant :: GenParser Char st Segment
unicodeBareConsonant =
  charTranslate [(ka, K),
                 (kha, Kh),
                 (ga, G),
                 (gha, Gh),
                 (velarNa, Ng),
                 (ca, C),
                 (cha, Ch),
                 (ja, J),
                 (jha, Jh),
                 (palatalNa, PalN),
                 (retroTa, RetT),
                 (retroTha, RetTh),
                 (retroDa, RetD),
                 (retroDha, RetDh),
                 (retroNa, RetN),
                 (ta, T),
                 (tha, Th),
                 (da, D),
                 (dha, Dh),
                 (na, N),
                 (pa, P),
                 (pha, Ph),
                 (ba, B),
                 (bha, Bh),
                 (ma, M),
                 (ya, Y),
                 (ra, R),
                 (la, L),
                 (va, V),
                 (palatalSa, PalS),
                 (retroSa, RetS),
                 (sa, S),
                 (ha, H)]

charTranslate :: [(Char, a)] -> GenParser Char st a
charTranslate [] = parserZero
charTranslate [(c, val) : rest] =
  (char c >> return val)
  <|> charTranslate rest

initA, initAA, combAA, initI, combI, initII, combII, initU, combU, initUU,
  combUU, initVocR, combVocR, initVocRR, combVocRR, initVocL, combVocL,
  initVocLL, combVocLL, initE, combE, initAI, combAI, initO, combO, initAU,
  combAU, visarga, anusvara, ka, kha, ga, gha, velarNa, ca, cha, ja, jha,
  palatalNa, retroTa, retroTha, retroDa, retroDha, retroNa, ta, tha, da, dha,
  na, pa, pha, ba, bha, ma, ya, ra, la, va, palatalSa, retroSa, sa, ha, virama,
  danda, doubleDanda :: Char
initA       = '\x0905'
initAA      = '\x0906'
combAA      = '\x093e'
initI       = '\x0907'
combI       = '\x093f'
initII      = '\x0908'
combII      = '\x0940'
initU       = '\x0909'
combU       = '\x0941'
initUU      = '\x090a'
combUU      = '\x0942'
initVocR    = '\x090b'
combVocR    = '\x0943'
initVocRR   = '\x0960'
combVocRR   = '\x0944'
initVocL    = '\x090c'
combVocL    = '\x0962'
initVocLL   = '\x0961'
combVocLL   = '\x0963'
initE       = '\x090f'
combE       = '\x0947'
initAI      = '\x0910'
combAI      = '\x0948'
initO       = '\x0913'
combO       = '\x094b'
initAU      = '\x0914'
combAU      = '\x094c'
visarga     = '\x0903'          -- combining form
anusvara    = '\x0902'          -- combining form
ka          = '\x0915'
kha         = '\x0916'
ga          = '\x0917'
gha         = '\x0918'
velarNa     = '\x0919'
ca          = '\x091a'
cha         = '\x091b'
ja          = '\x091c'
jha         = '\x091d'
palatalNa   = '\x091e'
retroTa     = '\x091f'
retroTha    = '\x0920'
retroDa     = '\x0921'
retroDha    = '\x0922'
retroNa     = '\x0923'
ta          = '\x0924'
tha         = '\x0925'
da          = '\x0926'
dha         = '\x0927'
na          = '\x0928'
pa          = '\x092a'
pha         = '\x092b'
ba          = '\x092c'
bha         = '\x092d'
ma          = '\x092e'
ya          = '\x092f'
ra          = '\x0930'
la          = '\x0932'
va          = '\x0935'
palatalSa   = '\x0936'
retroSa     = '\x0937'
sa          = '\x0938'
ha          = '\x0939'
virama      = '\x094d'
danda       = '\x0964'
doubleDanda = '\x0965'
