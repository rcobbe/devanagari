{-# LANGUAGE ScopedTypeVariables #-}

-- | This module defines the translations between devanagari representations
-- based on Unicode and on 'Segment'.

module Text.Devanagari.Unicode(
  -- * Parsing Unicode

  -- $parsingUnicode
  toSegments,
  -- * Converting to Unicode

  -- $parsingSegments
  fromSegments)
where

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Parsec.Prim
import Text.Parsec.String

import qualified Control.Exceptional as CE

import qualified Text.Devanagari.Exception as TDE
import Text.Devanagari.Segments

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
-- or a medial vowel.  So, we can divide the Unicode code points used for
-- Devanagari/Sanskrit into the following classes:
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
--     indicates hiatus.  In the former case, the hiatus is with the
--     consonant's implicit short A.)
--
--  2. Medial vowels must appear immediately after a consonant, with no
--     modifiers or virama allowed on the consonant.  (A modifier on the
--     consonant would indicate an implicit short a, leading to a misformed
--     hiatus.)
--
--  3. Vowel modifiers may not appear after viramas.  Modifiers on a consonant
--     apply to the consonant's inherent short A.
--
--  4. Viramas may appear only on consonants.
--
--  5. For now, assume that a vowel may have at most one modifier.  I don't
--     know whether we have attested text that contains an anusvara and a
--     visarga on the same vowel, and it simplifies the segment representation
--     to disallow the case.  If it /can/ happen, then the anusvara code point
--     should appear before the visarga, according to Apple's rendering logic.
--
-- This leads to the following grammar:
--
-- @ Word ::= InitialVowelMod+ Syllable* (Consonant Virama)*
--        | InitialVowelMod* Syllable+ (Consonant Virama)*
-- Syllable ::= (Consonant Virama)* Consonant MedialVowel? VowelMod? InitVowelMod*
-- InitialVowelMod ::= InitialVowel VowelMod?
-- VowelMod ::= Anusvara | Visarga@
--
-- The two productions for @word@ ensure that we have at least one initial
-- vowel or at least one syllable: this allows us to handle words that consist
-- entirely of an initial vowel, plus words that start with a consonant.
--
-- This is a graphical, rather than phonetic, notion of a syllable; it's
-- essentially the equivalent of the Sanskrit notion of an ak&#x1e63;ara, with a
-- slight modification to allow vowel hiatus.  (See
-- <http://en.wikipedia.org/wiki/Devanagari#Principle> for a definition of
-- \"ak&#x1e63;ara.\")
--
-- We allow vowel hiatus even though Sanskrit does not use it, largely so that
-- every list of 'Segment's has a corresponding Unicode representation.  A
-- modification of the grammar above that does not support hiatus follows:
--
-- @ Word ::= InitialVowelMod Syllable* (Consonant Virama)*
--        | Syllable+ (Consonant Virama)*
-- Syllable ::= (Consonant Virama)* Consonant MedialVowel? VowelMod?@
--
-- with the remaining productions as in the original grammar.

-- | Converts a Unicode string to a list of segments.  On error, throws
-- 'TDE.BadUnicode'.
toSegments :: String -> CE.Exceptional TDE.Error [Segment]
toSegments s =
  case parse unicode "" s of
    Left error ->
      case (errorMessages error) of
        [] -> CE.throw $ TDE.BadUnicode s "unknown error"
        (msg : _) -> CE.throw $ TDE.BadUnicode s (messageString msg)
    Right segments -> return segments

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

-- | Parse an initial unicode vowel with optional modifier.
initVowelWithMod :: GenParser Char st Segment
initVowelWithMod =
  do vowelCtor <- charTranslate initVowelMap
     mod <- vowelModifier
     return $ vowelCtor mod
  <?> "initial vowel"

-- | Parse a unicode syllable: leading consonants, plus one or more vowels.
-- Final consonants are handled by 'unicode' above.
syllable :: GenParser Char st [Segment]
syllable =
  do onset <- many (try consonantVirama)
     cnsnt <- consonant         -- must not have virama
     vowelCtor <- option A medialVowel
     mod <- vowelModifier
     hiatus <- many initVowelWithMod
     return $ concat [onset,
                      [cnsnt],
                      [vowelCtor mod],
                      hiatus]
  <?> "syllable"

-- | Parse a unicode medial vowel without modifiers.  (We separate the two
-- because modifiers can appear without an explicit medial vowel, for implicit
-- short A.)
medialVowel :: GenParser Char st (VowelMod -> Segment)
medialVowel =
  charTranslate medialVowelMap
  <?> "medial vowel"

-- | Parse a unicode vowel modifier.
vowelModifier :: GenParser Char st VowelMod
vowelModifier =
  (charTranslate vowelModMap
   <|> return NoMod)
  <?> "vowel modifier"

-- | Parse a unicode consonant followed by a virama.
consonantVirama :: GenParser Char st Segment
consonantVirama =
  do c <- consonant
     char virama <?> "virama"
     return c
  <?> "consonant with virama"

-- | Parse a unicode consonant; doesn't require a following virama.
consonant :: GenParser Char st Segment
consonant =
  charTranslate consonantMap
  <?> "consonant"

-- | Constructs a parser that recognizes any of a specific set of characters
-- and maps them to specified results.  Specifically, @charTranslate spec@
-- recognizes any of the characters in the domain of @spec@ and, on a
-- successful parse, returns the value associated with the match character.
charTranslate :: Map Char a -> GenParser Char st a
charTranslate map =
  do c <- oneOf (Map.keys map)
     return $ map ! c

initVowelMap :: Map Char (VowelMod -> Segment)
initVowelMap =
  Map.fromList [(initA, A),
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

medialVowelMap :: Map Char (VowelMod -> Segment)
medialVowelMap =
  Map.fromList [(combAA, AA),
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

vowelModMap :: Map Char VowelMod
vowelModMap = Map.fromList [(visarga, Visarga), (anusvara, Anusvara)]

consonantMap :: Map Char Segment
consonantMap =
  Map.fromList [(ka, K),
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

-- $parsingSegments
--
-- Converting segments to Unicode is more straightforward than the inverse.  In
-- general, the translation is straightforward: vowel segments map to Unicode
-- medial vowels plus any modifiers, and consonant segments map to Unicode
-- consonants plus viramas.  The only points that require some care are the
-- following:
--
-- * Vowel segments must map to Unicode initial vowels in certain contexts
--   (word initial, part of hiatus),
--
-- * Consonants before vowels must drop their viramas, and
--
-- * The segment 'A' does not appear medially in the Unicode version, and any
--   modifiers must be applied to the preceding consonant.
--
-- We use the following grammar:
--
-- @ Word ::= InitVowelWithMod+ Syllable* ConsonantClusterSegment*
--        | InitVowelWithMod* Syllable+ ConsonantClusterSegment*
-- Syllable ::= ConsonantClusterSegment* Consonant MedialVowelWithMod
--              InitVowelWithMod*
-- InitVowelWithMod ::= VowelSegment VowelMod?
-- MedialVowelWithMod ::= VowelSegment VowelMod?
-- Consonant ::= ConsonantSegment
-- ConsonantClusterSgment ::= Consonant not followed by a VowelSegment
-- VowelMod ::= Anusvara | Visarga@
--
-- Although @InitVowelWithMod@ and @MedialVowelWithMod@ describe identical
-- languages, the results are different; @InitVowelWithMod@ occurs in a context
-- where we need a Unicode code point for an initial vowel, and
-- @MedialVowelWithMod@ occurs where we need a Unicode combining vowel.
-- Similarly, the distinction between @Consonant@ and @ConsonantClusterSegment@
-- allows us to put the viramas in the right place.

-- | Segment parser's input type.  The Int supplies position information; we
-- use it as the column number in the generated 'SourcePos', so it should start
-- at 1 and increase by 1 with each successive input.
type SegmentToken = (Segment, Int)

-- | Converts a list of segments to its Unicode representation.
fromSegments :: [Segment] -> String
fromSegments segs =
  let parserResult :: Either ParseError String
      parserResult = parse segments "" (zip segs (enumFrom 1))
  in case parserResult of
    Left err -> error ("internal error: Text.Devanagari.Unicode.fromSegments: "
                       ++ show err)
    Right unicode -> unicode

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

-- | Parses a segment representing an initial vowel plus optional modifier;
-- returns unicode equivalent.
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

-- | Parses a segment representing a medial vowel plus initial modifier;
-- returns unicode equivalent.
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

-- | Parses segments representing a syllable.  Returns Unicode equivalent.
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

-- | Parses a single vowel segment; returns the matched segment.
vowelSegment :: GenParser SegmentToken st Segment
vowelSegment =
  token showToken posFromToken testToken <?> "segment denoting vowel"
    where
      showToken    (seg, pos) = show seg
      posFromToken (seg, pos) = setSourceColumn (initialPos "segment list") pos
      testToken    (seg, pos) = if isVowel seg then Just seg else Nothing

-- | Parses a consonant segment that appears in initial or medial position
-- within a cluster -- i.e., is not followed by a vowel and thus needs a
-- virama.  Returns unicode equivalent, including virama.
consonantClusterSegment :: GenParser SegmentToken st String
consonantClusterSegment =
  (do c <- consonantSegment
      notFollowedBy vowelSegment
      return [c, virama])
  <?> "segment denoting a consonant in initial/medial position in a cluster"

-- | Parses a consonant segment that appears immediately before a vowel and
-- thus does not need a virama.  Returns unicode equivalent.
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

-- | Constructs a parser that recognizes any of the segments in the domain of
-- the supplied map.  On success, returns the value associated with the matched
-- segment in the map.
segmentTranslate :: Map Segment a -> GenParser SegmentToken st a
segmentTranslate m =
  Map.foldrWithKey buildParser parserZero m
    where -- buildParser :: Segment -> a -> GenParser SegmentToken st a
          --             -> GenParser SegmentToken st a
      buildParser seg val parser =
        segment seg val <|> parser

-- | A parser that recognizes only a specified 'Segment' and returns the given
-- value on success.
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
-- rather than a map, because functions aren't in either 'Ord' or 'Eq'.)
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
