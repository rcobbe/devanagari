-- | This module defines the translations between devanagari representations
-- based on Unicode and on 'Text.Devanagari.Segments.Segment'.

module Text.Devanagari.Unicode(toSegments, fromSegments) where

-- XXX can we get rid of the qualification in the haddock comment at top of
-- file?

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

import Control.Monad.Error

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Prim
import Text.Parsec.String

import qualified Text.Devanagari.Exception as TDE
import Text.Devanagari.Segments

-- | Converts a Unicode string to a list of segments.  On error throws one of
-- the following:
-- *
toSegments :: String -> TDE.Exceptional [Segment]
toSegments s =
  case parse unicode "" s of
    Left error ->
      case (errorMessages error) of
        [] -> throwError $ TDE.BadUnicode s "unknown error"
        (msg : _) -> throwError $ TDE.BadUnicode s (messageString msg)
    Right segments -> Right segments

fromSegments :: [Segment] -> String
fromSegments = undefined

-- Character classes:
--    initial vowels
--    medial vowels
--    vowel modifiers (anusvara, visarga)
--    consonants
--    virama
--
-- Constraints:
--  1) Assume that a vowel may have at most one modifier.  It's not clear
--     whether, in original text, an anusvara and a visarga may appear on the
--     same vowel.  If this can happen, then the anusvara should appear
--     first in the Unicode stream, according to Apple's rendering logic.
--  2) Initial vowels may not appear after a virama.  Appearing after a medial
--     vowel or consonant, possibly with vowel modifier, indicates hiatus, with
--     short a in the latte case.
--  3) Medial vowels must appear after a consonant, with no modifiers allowed.
--     (A modifier would indicate an implicit [a], leading to an illegal
--     hiatus.)
--  4) Vowel modifiers may not appear after viramas.  Modifiers on a consonant
--     imply an [a].
--  5) Viramas may appear only on consonants.
--
-- Positively:
--  word ::= (InitVowel VowelMod?)* Syllable* (Consonant Virama)*
--  Syllable ::=
--    (Consonant Virama)* Consonant MedialVowel? VowelMod?
--    (InitVowel VowelMod?)*
-- need to ensure that word isn't empty --
--  word ::= (InitVowel VowelMod?)+ Syllable* (Consonant Virama)*
--         | (InitVowel VowelMod?)* Syllable+ (Consonant Virama)*

unicode :: GenParser Char st [Segment]
unicode =
  try (do initVowels <- many1 initVowelWithMod
          syllables <- many (try syllable)
          coda <- many consonantVirama
          eof
          return $ concat (initVowels : syllables ++ [coda]))
  <|>
  (do initVowels <- many initVowelWithMod
      syllables <- many1 (try syllable)
      coda <- many consonantVirama
      eof
      return $ concat (initVowels : syllables ++ [coda]))
  <?> "initial vowel or consonant"

initVowelWithMod :: GenParser Char st Segment
initVowelWithMod =
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
     mod <- vowelModifier
     return $ vowelCtor mod
  <?> "initial vowel"

syllable :: GenParser Char st [Segment]
syllable =
  do onset <- many (try consonantVirama)
     cnsnt <- consonant
     medVowel <- optionMaybe medialVowel
     mod <- option NoMod vowelModifier
     hiatus <- many initVowelWithMod
     return $ concat [onset,
                      [cnsnt],
                      case medVowel of
                        Just vc -> [vc mod]
                        Nothing -> [A mod],
                      hiatus]
  <?> "syllable"

medialVowel :: GenParser Char st (VowelMod -> Segment)
medialVowel =
  charTranslate [(combAA, AA),
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
  <?> "medial vowel"

vowelModifier :: GenParser Char st VowelMod
vowelModifier =
  (charTranslate [(visarga, Visarga), (anusvara, Anusvara)]
   <|> return NoMod)
  <?> "vowel modifier"

consonantVirama :: GenParser Char st Segment
consonantVirama =
  do c <- consonant
     char virama <?> "virama"
     return c
  <?> "consonant with virama"

consonant :: GenParser Char st Segment
consonant =
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
  <?> "consonant"

charTranslate :: [(Char, a)] -> GenParser Char st a
charTranslate [] = parserZero
charTranslate ((c, val) : rest) =
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
