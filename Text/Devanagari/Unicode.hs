{-# LANGUAGE ScopedTypeVariables #-}

-- | This module defines the translations between devanagari representations
-- based on Unicode and on 'Segment'.

module Text.Devanagari.Unicode(
  -- * Parsing Unicode

  -- $parsingUnicode
  toSegments,
  -- * Converting to Unicode
  fromSegments)
where

import Control.Monad.Error
import Data.Map (Map)
import qualified Data.Map as Map

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Parsec.Prim
import Text.Parsec.String

import qualified Text.Devanagari.Exception as TDE
import Text.Devanagari.Segments

-- | Converts a Unicode string to a list of segments.  On error, throws
-- 'TDE.BadUnicode'.
toSegments :: String -> TDE.Exceptional [Segment]
toSegments s =
  case parse unicode "" s of
    Left error ->
      case (errorMessages error) of
        [] -> throwError $ TDE.BadUnicode s "unknown error"
        (msg : _) -> throwError $ TDE.BadUnicode s (messageString msg)
    Right segments -> Right segments

-- Character classes in Unicode's representation of Devanagari text:
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
--     short A in the latter case.
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

-- | Parse a single unicode word into segments.  If the word does not start
-- with an initial vowel, it must contain at least one syllable.
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

-- | Parse an initial vowel with optional modifier.
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

-- | Parse a syllable: leading consonants, plus one or more vowels.  Final
-- consonants are handled by 'unicode' above.
syllable :: GenParser Char st [Segment]
syllable =
  do onset <- many (try consonantVirama)
     cnsnt <- consonant         -- may not have virama
     medVowel <- optionMaybe medialVowel -- XXX use option A instead of optionMaybe
     mod <- option NoMod vowelModifier
     hiatus <- many initVowelWithMod
     return $ concat [onset,
                      [cnsnt],
                      case medVowel of
                        Just vc -> [vc mod]
                        Nothing -> [A mod],
                      hiatus]
  <?> "syllable"

-- | Parse a medial vowel without modifiers.  (We separate the two because
-- modifiers can appear without an explicit medial vowel, for implicit short A.)
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

-- XXX callers to vowelModifier shouldn't use option

-- | Parse a vowel modifier.
vowelModifier :: GenParser Char st VowelMod
vowelModifier =
  (charTranslate [(visarga, Visarga), (anusvara, Anusvara)]
   <|> return NoMod)
  <?> "vowel modifier"

-- | Parse a consonant followed by a virama.
consonantVirama :: GenParser Char st Segment
consonantVirama =
  do c <- consonant
     char virama <?> "virama"
     return c
  <?> "consonant with virama"

-- | Parse a consonant; doesn't require a following virama.
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

-- | Constructs a parser that recognizes any of a specific set of characters
-- and maps them to specified results.  Specifically, 'charTranslate spec'
-- recognizes any of the characters in the domain of 'spec' and, on a
-- successful parse, returns the value associated with the match character.
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

-- $parsingUnicode
-- Converting Unicode devanagari text to a segment-based representation
-- requires getting lots of details right, due to the differences between the
-- two representations.  In the 'Segment'-based representation, consonants do
-- not possess an inherent vowel, and the representation therefore does not
-- need a virama.  Additionally, we do not distinguish between initial and
-- medial vowels, and we treat the anusvara and visarga as modifiers on the
-- previous vowel.  At the moment, we allow either anusvara or visarga to
-- appear on a particular vowel, but never both at the same time.
--
-- In the Unicode representation, however, initial and medial vowels have
-- separate representations.  Consonants possess an inherent short A,
-- which is cancelled whenever the consonant is followed by a virama (U+094D)
-- or a medial vowel.  So, we can divide the Devanagari code points used for
-- Sanskrit into the following classes:
--
-- * initial vowels
--
-- * medial vowels
--
-- * vowel modifiers: anusvara, visarga  (These are phonetically consonants,
--   but they may only appear immediately after a vowel, so treating them as a
--   separate class simplifies the implementation.)
--
-- * consonants  (A vowel modifier may appear immediately after a consonant,
--   in which case the modifier applies to the consonant's inherent short A.)
--
-- * virama
--
-- Unicode devanagari text obeys the following constraints:
--
--  1. Initial vowels may not appear after a virama.  (An initial vowel
--     appearing after a consonant or medial vowel, possibly with modifier,
--     indicates hiatus.)
--
--  2. Medial vowels must appear immediately after a consonant, with no
--     modifiers or virama allowed on the consonant.  (A modifier on the
--     consonant would indicate an implicit [a], leading to a misformed
--     hiatus.)
--
--  3. Vowel modifiers may not appear after viramas.  Modifiers on a consonant
--     apply to the consonant's inherent short A.
--
--  4. Viramas may appear only on consonants.
--
-- This leads to the following grammar:
--
-- @ Word ::= InitialVowelMod+ Syllable* (Consonant Virama)*
--        | InitialVowelMod* Syllable+ (Consonant Virama)*
-- Syllable ::= (Consonant Virama)* Consonant MedialVowel? VowelMod? InitVowelMod*
-- InitialVowelMod ::= InitialVowel VowelMod?
-- VowelMod ::= Anusvara | Visarga@
--
-- The complexity in the definition of @word@ ensures that we have at least
-- one initial vowel or at least one syllable: this allows us to handle words
-- that consist entirely of an initial vowel, plus words that start with a
-- consonant.
--
-- This is a graphical, rather than phonetic, notion of a syllable; it's
-- essentially the equivalent of the Sanskrit notion of an ak&#x1e63;ara, with a
-- slight modification to allow vowel hiatus.  (See
-- <http://en.wikipedia.org/wiki/Devanagari#Principle> for a definition of
-- \"ak&#x1e63;ara.\")
--
-- We allow vowel hiatus even though Sanskrit does not use it, largely so that
-- every list of 'Segment's has a corresponding Unicode represenation.  A
-- modification of the grammar above that does not support hiatus follows:
--
-- @ Word ::= InitialVowelMod Syllable* (Consonant Virama)*
--        | Syllable? (Consonant Virama)*
-- Syllable ::= (Consonant Virama)* Consonant MedialVowel? VowelMod?@
--
-- with the remaining productions as in the original grammar.

-- | Converts a list of segments to its Unicode representation.
fromSegments :: [Segment] -> String
fromSegments segs =
  let parserResult :: Either ParseError String
      parserResult = parse segments "" (zip segs (enumFrom 1))
  in case parserResult of
    Left err -> error (show err)
    Right unicode -> unicode

type SegmentToken = (Segment, Int)

-- SWord ::= InitVowelWithMod+ Syllable* Consonant*
--         | InitVowelWithMod* Syllable+ Consonant*
-- Syllable ::= Consonant+ MedialVowelWithMod InitialVowelWithMod*

-- | Parses a word of segments; returns the Unicode equivalent.
segments :: GenParser SegmentToken st String
segments =
  try (do vs :: [String] <- many1 segmentInitVowelWithMod
          syllables :: [String] <- many (try segmentSyllable)
          coda :: [String] <- many consonantClusterSegment
          eof
          return $ concat (vs ++ syllables ++ coda))
  <|>
  do vs :: [String] <- many segmentInitVowelWithMod
     syllables :: [String] <- many1 (try segmentSyllable)
     coda :: [String] <- many consonantClusterSegment
     eof
     return $ concat (vs ++ syllables ++ coda)
  <?> "word of segments"

-- | Parses a segment representing an initial vowel plus modifier; returns
-- unicode equivalent.
segmentInitVowelWithMod :: GenParser SegmentToken st String
segmentInitVowelWithMod =
  segmentTranslate
  (makeVowelSegmentMap
   [(A, [initA]),
    (AA, [initAA]),
    (I, [initI]),
    (II, [initII]),
    (U, [initU]),
    (UU, [initUU]),
    (VocR, [initVocR]),
    (VocRR, [initVocRR]),
    (VocL, [initVocL]),
    (VocLL, [initVocLL]),
    (E, [initE]),
    (AI, [initAI]),
    (O, [initO]),
    (AU, [initAU])])
  <?> "segment denoting initial vowel"

segmentMedialVowelWithMod :: GenParser SegmentToken st String
segmentMedialVowelWithMod =
  segmentTranslate
  (makeVowelSegmentMap
   [(A, []),
    (AA, [combAA]),
    (I, [combI]),
    (II, [combII]),
    (U, [combU]),
    (UU, [combUU]),
    (VocR, [combVocR]),
    (VocRR, [combVocRR]),
    (VocL, [combVocL]),
    (VocLL, [combVocLL]),
    (E, [combE]),
    (AI, [combAI]),
    (O, [combO]),
    (AU, [combAU])])
  <?> "segment denoting medial vowel"

-- Syllable ::= Consonant+ MedialVowelWithMod InitialVowelWithMod*
segmentSyllable :: GenParser SegmentToken st String
segmentSyllable =
  (do leadingConsonants :: [String] <- many (try consonantClusterSegment)
      finalOnsetConsonant :: Char <- consonantSegment
      -- remember that short a is represented *explicitly* in Segments.
      vowelStr :: String <- segmentMedialVowelWithMod
      hiatus :: [String] <- many segmentInitVowelWithMod
      return $ concat (leadingConsonants
                       ++ [finalOnsetConsonant : vowelStr]
                       ++ hiatus))
  <?> "syllable of segments"

vowelSegment :: GenParser SegmentToken st Segment
vowelSegment =
  token showToken posFromToken testToken <?> "segment denoting vowel"
    where
      showToken    (seg, pos) = show seg
      posFromToken (seg, pos) = setSourceColumn (initialPos "segment list") pos
      testToken    (seg, pos) = if isVowel seg then Just seg else Nothing

consonantClusterSegment :: GenParser SegmentToken st String
consonantClusterSegment =
  (do c <- consonantSegment
      notFollowedBy vowelSegment
      return [c, virama])
  <?> "segment denoting a consonant in initial/medial position in a cluster"

consonantSegment :: GenParser SegmentToken st Char
consonantSegment =
  segmentTranslate
  (Map.fromList [(K, ka),
                 (Kh, kha),
                 (G, ga),
                 (Gh, gha),
                 (Ng, velarNa),
                 (C, ca),
                 (Ch, cha),
                 (J, ja),
                 (Jh, jha),
                 (PalN, palatalNa),
                 (RetT, retroTa),
                 (RetTh, retroTha),
                 (RetD, retroDa),
                 (RetDh, retroDha),
                 (RetN, retroNa),
                 (T, ta),
                 (Th, tha),
                 (D, da),
                 (Dh, dha),
                 (N, na),
                 (P, pa),
                 (Ph, pha),
                 (B, ba),
                 (Bh, bha),
                 (M, ma),
                 (Y, ya),
                 (R, ra),
                 (L, la),
                 (V, va),
                 (PalS, palatalSa),
                 (RetS, retroSa),
                 (S, sa),
                 (H, ha)])
  <?> "segment denoting a cosonant"

segmentTranslate :: Map Segment a -> GenParser SegmentToken st a
segmentTranslate m =
  Map.foldrWithKey buildParser parserZero m
    where -- buildParser :: Segment -> a -> GenParser SegmentToken st a
          --             -> GenParser SegmentToken st a
      buildParser seg val parser =
        segment seg val <|> parser

segment :: Segment -> a -> GenParser SegmentToken st a
segment desiredSegment val =
  token showToken posFromToken testToken
  where
    showToken    (seg, pos) = show seg
    posFromToken (seg, pos) = setSourceColumn (initialPos "segment list") pos
    testToken    (seg, pos) =
      if seg == desiredSegment
      then Just val
      else Nothing

-- | Builds a map from vowel segment to string, given a map from vowel
-- constructor to vowel character.  (Have to represent the input as an alist,
-- rather than a map, because functions aren't in either Ord or Eq.)
makeVowelSegmentMap :: [(VowelMod -> Segment, String)] -> Map Segment String
makeVowelSegmentMap vowelCtors =
  foldr addVowelToMap Map.empty vowelCtors
  where addVowelToMap :: (VowelMod -> Segment, String) -> Map Segment String
                         -> Map Segment String
        addVowelToMap (vowelCtor, vowelStr) map =
          Map.insert (vowelCtor NoMod) vowelStr
            (Map.insert (vowelCtor Visarga) (vowelStr ++ [visarga])
               (Map.insert (vowelCtor Anusvara) (vowelStr ++ [anusvara])
                  map))
