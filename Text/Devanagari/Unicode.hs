module Text.Devanagari.Unicode(toPhonemic, fromPhonemic)
where

import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Trie (Trie)
import qualified Data.Trie as T

import qualified Text.Devanagari.Phonemic as P

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
-- means that toPhonemic and fromPhonemic are effectively inverses.  Also,
-- since the phonemic representation allows hiatus, supporting it here and in
-- fromPhonemic gives us a unicode representation of any phonemic string.)
-- Returns Nothing on encoding error:
--   * unsupported char in input
--   * medial vowel or virama anywhere except after consonant
--   * visarga, anusvara anywhere except after vowel
toPhonemic :: String -> Maybe [P.Phoneme]
toPhonemic [] = Just []
toPhonemic (c : cs)
  | c `M.member` initVowels = toPhonemicVowel (initVowels M.! c) cs
  | c `M.member` consonants = toPhonemicConsonant (consonants M.! c) cs
  | otherwise = Nothing

-- Convert from unicode, after detecting a vowel (either initial or medial).
-- Handle any vowel modifiers, then continue on.  First arg must be
-- constructor corresponding to vowel that we just saw.
toPhonemicVowel :: (P.VowelMod -> P.Phoneme) -> String -> Maybe [P.Phoneme]
toPhonemicVowel v [] = Just [v P.NoMod]
toPhonemicVowel v s@(c : cs)
  | c == visarga = (v P.Visarga) `mcons` toPhonemic cs
  | c == anusvara = (v P.Anusvara) `mcons` toPhonemic cs
  | otherwise = (v P.NoMod) `mcons` toPhonemic s
                -- use of toPhonemic in last case allows hiatus; to signal
                -- error, use toPhonemicNoVowel.

-- Convert from unicode, after seeing a consonant.  Handle complicated cases,
-- then continue on.  First arg is consonant ctor.  Cases:
--   1. end of word: add implicit a
--   2. virama: bare consonant, then look for either end of word or another
--         consonant.  (Vowels after viramas disallowed.)
--   3. consonant: add implicit a, then continue with following consonant.
--   4. visarga, anusvara: add implicit a with modifier, then continue.
--   5. medial vowel: bare consonant, then call toPhonemicVowel to handle
--          modifiers after vowel.
--   6. initial vowel: add implicit a, then continue with toPhonemic to handle
--          hiatus.
toPhonemicConsonant :: P.Phoneme -> String -> Maybe [P.Phoneme]
toPhonemicConsonant p [] = Just [p, P.A P.NoMod]
toPhonemicConsonant p s@(c : cs)
  | c == virama = p `mcons` toPhonemicNoVowel cs
  | c `M.member` consonants = mcons p (mcons (P.A P.NoMod) (toPhonemic s))
  | c == visarga = mcons p (mcons (P.A P.Visarga) (toPhonemic cs))
  | c == anusvara = mcons p (mcons (P.A P.Anusvara) (toPhonemic cs))
  | c `M.member` medialVowels =
    p `mcons` toPhonemicVowel (medialVowels M.! c) cs
  | c `M.member` initVowels = mcons p (mcons (P.A P.NoMod) (toPhonemic s))
  | otherwise = Nothing

-- Convert from unicode, after seeing consonant + virama.  Must find consonant
-- or end of word here.
toPhonemicNoVowel :: String -> Maybe [P.Phoneme]
toPhonemicNoVowel [] = Just []
toPhonemicNoVowel (c : cs)
  | c `M.member` consonants = toPhonemicConsonant (consonants M.! c) cs
  | otherwise = Nothing

mcons :: a -> Maybe [a] -> Maybe [a]
mcons _ Nothing = Nothing
mcons x (Just xs) = Just (x:xs)

initVowels :: Map Char (P.VowelMod -> P.Phoneme)
initVowels = M.fromList (
  [(initA, P.A),
   (initAA, P.AA),
   (initI, P.I),
   (initII, P.II),
   (initU, P.U),
   (initUU, P.UU),
   (initVocR, P.VocR),
   (initVocRR, P.VocRR),
   (initVocL, P.VocL),
   (initVocLL, P.VocLL),
   (initE, P.E),
   (initAI, P.AI),
   (initO, P.O),
   (initAU, P.AU)])

medialVowels :: Map Char (P.VowelMod -> P.Phoneme)
medialVowels = M.fromList (
  [(combAA, P.AA),
   (combI, P.I),
   (combII, P.II),
   (combU, P.U),
   (combUU, P.UU),
   (combVocR, P.VocR),
   (combVocRR, P.VocRR),
   (combVocL, P.VocL),
   (combVocLL, P.VocLL),
   (combE, P.E),
   (combAI, P.AI),
   (combO, P.O),
   (combAU, P.AU)])

consonants :: Map Char P.Phoneme
consonants = M.fromList (
  [(ka, P.K),
   (kha, P.Kh),
   (ga, P.G),
   (gha, P.Gh),
   (velarNa, P.Ng),
   (ca, P.C),
   (cha, P.Ch),
   (ja, P.J),
   (jha, P.Jh),
   (palatalNa, P.PalN),
   (retroTa, P.RetT),
   (retroTha, P.RetTh),
   (retroDa, P.RetD),
   (retroDha, P.RetDh),
   (retroNa, P.RetN),
   (ta, P.T),
   (tha, P.Th),
   (da, P.D),
   (dha, P.Dh),
   (na, P.N),
   (pa, P.P),
   (pha, P.Ph),
   (ba, P.B),
   (bha, P.Bh),
   (ma, P.M),
   (ya, P.Y),
   (ra, P.R),
   (la, P.L),
   (va, P.V),
   (palatalSa, P.PalS),
   (retroSa, P.RetS),
   (sa, P.S),
   (ha, P.H)])

fromPhonemic :: [P.Phoneme] -> String
fromPhonemic [] = []
fromPhonemic (p : ps)
  | p `M.member` phonemicInitVowels =
    (phonemicInitVowels M.! p) ++ fromPhonemic ps
  | p `M.member` phonemicConsonants =
    (phonemicConsonants M.! p) : fromPhonemicAfterConsonant ps
  | otherwise = error "fromPhonemic internal error"

fromPhonemicAfterConsonant :: [P.Phoneme] -> String
fromPhonemicAfterConsonant [] = [virama]
fromPhonemicAfterConsonant (p : ps)
  | p `M.member` phonemicMedialVowels =
    (phonemicMedialVowels M.! p) ++ fromPhonemic ps
  | p `M.member` phonemicConsonants =
    virama : (phonemicConsonants M.! p) : fromPhonemicAfterConsonant ps
  | otherwise = error "fromPhonemic internal error"

phonemicInitVowels :: Map P.Phoneme String
phonemicInitVowels =
  foldl' addPhonemicVowelEntry M.empty [
    (P.A, [initA]),
    (P.AA, [initAA]),
    (P.I, [initI]),
    (P.II, [initII]),
    (P.U, [initU]),
    (P.UU, [initUU]),
    (P.VocR, [initVocR]),
    (P.VocRR, [initVocRR]),
    (P.VocL, [initVocL]),
    (P.VocLL, [initVocLL]),
    (P.E, [initE]),
    (P.AI, [initAI]),
    (P.O, [initO]),
    (P.AU, [initAU])]

phonemicMedialVowels :: Map P.Phoneme String
phonemicMedialVowels =
  foldl' addPhonemicVowelEntry M.empty [
    (P.A, []),
    (P.AA, [combAA]),
    (P.I, [combI]),
    (P.II, [combII]),
    (P.U, [combU]),
    (P.UU, [combUU]),
    (P.VocR, [combVocR]),
    (P.VocRR, [combVocRR]),
    (P.VocL, [combVocL]),
    (P.VocLL, [combVocLL]),
    (P.E, [combE]),
    (P.AI, [combAI]),
    (P.O, [combO]),
    (P.AU, [combAU])]

addPhonemicVowelEntry :: Map P.Phoneme String
                         -> (P.VowelMod -> P.Phoneme, String)
                         -> Map P.Phoneme String
addPhonemicVowelEntry m (p, u) =
  M.insert (p P.NoMod) u (
    M.insert (p P.Visarga) (u ++ [visarga]) (
       M.insert (p P.Anusvara) (u ++ [anusvara]) m))

phonemicConsonants :: Map P.Phoneme Char
phonemicConsonants = M.fromList [
  (P.K, ka),
  (P.Kh, kha),
  (P.G, ga),
  (P.Gh, gha),
  (P.Ng, velarNa),
  (P.C, ca),
  (P.Ch, cha),
  (P.J, ja),
  (P.Jh, jha),
  (P.PalN, palatalNa),
  (P.RetT, retroTa),
  (P.RetTh, retroTha),
  (P.RetD, retroDa),
  (P.RetDh, retroDha),
  (P.RetN, retroNa),
  (P.T, ta),
  (P.Th, tha),
  (P.D, da),
  (P.Dh, dha),
  (P.N, na),
  (P.P, pa),
  (P.Ph, pha),
  (P.B, ba),
  (P.Bh, bha),
  (P.M, ma),
  (P.Y, ya),
  (P.R, ra),
  (P.L, la),
  (P.V, va),
  (P.PalS, palatalSa),
  (P.RetS, retroSa),
  (P.S, sa),
  (P.H, ha)]
