module Text.Devanagari.Unicode(toSegments, fromSegments)
where

import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Trie (Trie)
import qualified Data.Trie as T

import qualified Text.Devanagari.Segments as S

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

-- Convert from unicode, at either beginning of word or after medial vowel.
-- (Using this function after a medial vowel allows internal hiatus, which
-- means that toSegments and fromSegments are effectively inverses.  Also,
-- since the segment representation allows hiatus, supporting it here and in
-- fromSegments gives us a unicode representation of any segmental string.)
-- Returns Nothing on encoding error:
--   * unsupported char in input
--   * medial vowel or virama anywhere except after consonant
--   * visarga, anusvara anywhere except after vowel
toSegments :: String -> Maybe [S.Segment]
toSegments [] = Just []
toSegments (char : chars)
  | char `M.member` initVowelCtors =
    toSegmentsVowel (initVowelCtors M.! char) chars
  | char `M.member` consonants =
    toSegmentsConsonant (consonants M.! char) chars
  | otherwise = Nothing

-- Convert from unicode, after detecting a Unicode vowel (either initial or
-- medial).  Handle any vowel modifiers, then continue on.  First arg must be
-- constructor corresponding to vowel that we just saw.
toSegmentsVowel :: (S.VowelMod -> S.Segment) -> String -> Maybe [S.Segment]
toSegmentsVowel vowelCtor [] = Just [vowelCtor S.NoMod]
toSegmentsVowel vowelCtor str@(char : chars)
  | char == visarga = (vowelCtor S.Visarga) `mcons` toSegments chars
  | char == anusvara = (vowelCtor S.Anusvara) `mcons` toSegments chars
  | otherwise =
    -- use of toSegments in this case allows vowel hiatus; to signal error on
    -- consecutive vowels instead of hiatus, use toSegmentsNoVowel.
    (vowelCtor S.NoMod) `mcons` toSegments str

-- Convert from unicode, after seeing a Unicode consonant.  Handle complicated
-- cases, then continue on.  First arg is consonant segment.  Cases:
--   1. end of word: add implicit a
--   2. virama: bare consonant, then look for either end of word or another
--         consonant.  (Vowels after viramas disallowed.)
--   3. consonant: add implicit a, then continue with following consonant.
--   4. visarga, anusvara: add implicit a with modifier, then continue.
--   5. medial vowel: add bare consonant, then call toSegmentsVowel to handle
--          modifiers after vowel.
--   6. initial vowel: add implicit a, then continue with toSegments to handle
--          hiatus.
toSegmentsConsonant :: S.Segment -> String -> Maybe [S.Segment]
toSegmentsConsonant cnsnt [] = Just [cnsnt, S.A S.NoMod]
toSegmentsConsonant cnsnt str@(char : chars)
  | char == virama = cnsnt `mcons` toSegmentsNoVowel chars
  | char `M.member` consonants =
    mcons cnsnt (mcons (S.A S.NoMod) (toSegments str))
  | char == visarga = mcons cnsnt (mcons (S.A S.Visarga) (toSegments chars))
  | char == anusvara = mcons cnsnt (mcons (S.A S.Anusvara) (toSegments chars))
  | char `M.member` medialVowels =
    cnsnt `mcons` toSegmentsVowel (medialVowels M.! char) chars
  | char `M.member` initVowelCtors =
    mcons cnsnt (mcons (S.A S.NoMod) (toSegments str))
  | otherwise = Nothing

-- Convert from unicode, after seeing consonant + virama.  Must find consonant
-- or end of word here.
toSegmentsNoVowel :: String -> Maybe [S.Segment]
toSegmentsNoVowel [] = Just []
toSegmentsNoVowel (char : chars)
  | char `M.member` consonants = toSegmentsConsonant (consonants M.! char) chars
  | otherwise = Nothing

-- half-lifting of cons into the Maybe.
mcons :: a -> Maybe [a] -> Maybe [a]
mcons _ Nothing = Nothing
mcons x (Just xs) = Just (x:xs)

-- maps unicode chars denoting word-initial Devanagari vowels to segment
-- constructors (parameterized over vowel modifiers).
initVowelCtors :: Map Char (S.VowelMod -> S.Segment)
initVowelCtors = M.fromList (
  [(initA, S.A),
   (initAA, S.AA),
   (initI, S.I),
   (initII, S.II),
   (initU, S.U),
   (initUU, S.UU),
   (initVocR, S.VocR),
   (initVocRR, S.VocRR),
   (initVocL, S.VocL),
   (initVocLL, S.VocLL),
   (initE, S.E),
   (initAI, S.AI),
   (initO, S.O),
   (initAU, S.AU)])

-- maps unicode chars denoting medial Devanagari vowels to segment constructors
-- (parameterized over vowel modifiers)
medialVowels :: Map Char (S.VowelMod -> S.Segment)
medialVowels = M.fromList (
  [(combAA, S.AA),
   (combI, S.I),
   (combII, S.II),
   (combU, S.U),
   (combUU, S.UU),
   (combVocR, S.VocR),
   (combVocRR, S.VocRR),
   (combVocL, S.VocL),
   (combVocLL, S.VocLL),
   (combE, S.E),
   (combAI, S.AI),
   (combO, S.O),
   (combAU, S.AU)])

-- maps unicode chars denoting Devanagari consonants to segments.
consonants :: Map Char S.Segment
consonants = M.fromList (
  [(ka, S.K),
   (kha, S.Kh),
   (ga, S.G),
   (gha, S.Gh),
   (velarNa, S.Ng),
   (ca, S.C),
   (cha, S.Ch),
   (ja, S.J),
   (jha, S.Jh),
   (palatalNa, S.PalN),
   (retroTa, S.RetT),
   (retroTha, S.RetTh),
   (retroDa, S.RetD),
   (retroDha, S.RetDh),
   (retroNa, S.RetN),
   (ta, S.T),
   (tha, S.Th),
   (da, S.D),
   (dha, S.Dh),
   (na, S.N),
   (pa, S.P),
   (pha, S.Ph),
   (ba, S.B),
   (bha, S.Bh),
   (ma, S.M),
   (ya, S.Y),
   (ra, S.R),
   (la, S.L),
   (va, S.V),
   (palatalSa, S.PalS),
   (retroSa, S.RetS),
   (sa, S.S),
   (ha, S.H)])

fromSegments :: [S.Segment] -> String
fromSegments [] = []
fromSegments (seg : segs)
  | seg `M.member` initVowelSegments =
    (initVowelSegments M.! seg) ++ fromSegments segs
  | seg `M.member` consonantSegments =
    (consonantSegments M.! seg) : fromSegmentsAfterConsonant segs
  | otherwise = error "fromSegments internal error"

fromSegmentsAfterConsonant :: [S.Segment] -> String
fromSegmentsAfterConsonant [] = [virama]
fromSegmentsAfterConsonant (seg : segs)
  | seg `M.member` medialVowelSegments =
    (medialVowelSegments M.! seg) ++ fromSegments segs
  | seg `M.member` consonantSegments =
    virama : (consonantSegments M.! seg) : fromSegmentsAfterConsonant segs
  | otherwise = error "fromSegments internal error"

-- maps vowel segments to their word-initial unicode equivalents, including all
-- vowel modifiers.
initVowelSegments :: Map S.Segment String
initVowelSegments =
  foldl' addSegmentVowelEntry M.empty [
    (S.A, [initA]),
    (S.AA, [initAA]),
    (S.I, [initI]),
    (S.II, [initII]),
    (S.U, [initU]),
    (S.UU, [initUU]),
    (S.VocR, [initVocR]),
    (S.VocRR, [initVocRR]),
    (S.VocL, [initVocL]),
    (S.VocLL, [initVocLL]),
    (S.E, [initE]),
    (S.AI, [initAI]),
    (S.O, [initO]),
    (S.AU, [initAU])]

-- maps vowel segments to their medial unicode equivalents, including all vowel
-- modifiers.
medialVowelSegments :: Map S.Segment String
medialVowelSegments =
  foldl' addSegmentVowelEntry M.empty [
    (S.A, []),
    (S.AA, [combAA]),
    (S.I, [combI]),
    (S.II, [combII]),
    (S.U, [combU]),
    (S.UU, [combUU]),
    (S.VocR, [combVocR]),
    (S.VocRR, [combVocRR]),
    (S.VocL, [combVocL]),
    (S.VocLL, [combVocLL]),
    (S.E, [combE]),
    (S.AI, [combAI]),
    (S.O, [combO]),
    (S.AU, [combAU])]

-- addSegmentVowelEntry map (vowelCtor, unicodeStr)
-- Extends map with all 3 entries corresponding to vowelCtor and unicodeStr: no
-- mod, visarga, anusvara.   We expect a string rather than a char for the
-- unicode representation, because medial A has an empty unicode
-- representation, and [Char] is easier to deal with than a Maybe Char in this
-- case.
addSegmentVowelEntry :: Map S.Segment String
                        -> (S.VowelMod -> S.Segment, String)
                        -> Map S.Segment String
addSegmentVowelEntry map (vowelCtor, unicode) =
  M.insert (vowelCtor S.NoMod) unicode (
    M.insert (vowelCtor S.Visarga) (unicode ++ [visarga]) (
       M.insert (vowelCtor S.Anusvara) (unicode ++ [anusvara]) map))

-- maps consonantal segments to their unicode representations
consonantSegments :: Map S.Segment Char
consonantSegments = M.fromList [
  (S.K, ka),
  (S.Kh, kha),
  (S.G, ga),
  (S.Gh, gha),
  (S.Ng, velarNa),
  (S.C, ca),
  (S.Ch, cha),
  (S.J, ja),
  (S.Jh, jha),
  (S.PalN, palatalNa),
  (S.RetT, retroTa),
  (S.RetTh, retroTha),
  (S.RetD, retroDa),
  (S.RetDh, retroDha),
  (S.RetN, retroNa),
  (S.T, ta),
  (S.Th, tha),
  (S.D, da),
  (S.Dh, dha),
  (S.N, na),
  (S.P, pa),
  (S.Ph, pha),
  (S.B, ba),
  (S.Bh, bha),
  (S.M, ma),
  (S.Y, ya),
  (S.R, ra),
  (S.L, la),
  (S.V, va),
  (S.PalS, palatalSa),
  (S.RetS, retroSa),
  (S.S, sa),
  (S.H, ha)]
