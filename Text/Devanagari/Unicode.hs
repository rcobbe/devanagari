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
-- (Using this function after a medial vowel allows internal hiatus; see
-- comments on fromPhonemic for why we choose to allow this.)
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

-- XXX test coverage for visarga, anusvara after /a/ as well as other vowels.
-- XXX test coverage:
--   word initial vowel (/a/ and otherwise)
--   word initial hiatus (first vowel /a/ and otherwise)
--   mid-word hiatus (first vowel /a/ and otherwise)
--   hiatus with visarga, anusvara
--   visarga, anusvara after /a/, otherwise; initial, medial, word-final
--   all consonants
--   all medial vowels
--   all initial vowels

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

-- only illegal combination in phonemic representation is hiatus.  Suggests
-- that XXX we should support hiatus when converting unicode -> phonemic,
-- so we get symmetry but don't have to return a Maybe String here.
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

{-

-- interface options considered:
-- 1) toPhonemic :: String -> Maybe [[P.Phoneme]]
--          splits at word boundaries, discards whitespace; None on failure
-- 2) toPhonemic :: String -> Maybe ([P.Phoneme], String)
--          convert what we can, return the rest; None if no conversion possible
-- 3) toPhonemic :: String -> ([P.Phoneme], String)
--          convert what we can; return the rest; return ([], arg) on failure
-- going w/ #2; seems clearest
-- no error checking for now.  Examples of things we might check:
--   - only initial vowels in word-initial position
--   - only combining vowels medially/finally
--   - visarga, anusvara must immediately follow vowels
-- ignore word-initial special casing (i.e., only combining forms legal in
--   medial, final position)
-- #1 would have involved incorporating notion of whitespace into this module.
-- Not only is this a loss of modularity & generality, but it's a pain, and I
-- don't know enough to predict the definition of whitespace yet.

-- Converts as long a prefix of its input to the phonemic representation as
-- possible; returns conversion and unconverted input.  If no (non-empty)
-- prefix is convertible, returns Nothing.
toPhonemic :: String -> Maybe ([P.Phoneme], String)
toPhonemic [] = Nothing
toPhonemic s =
  -- Maybe monad
  do (s', phonemes) <- T.matchPrefix unicodeTrie s
     let (phonemes', rest) = toPhonemicMedial s'
     return (phonemes ++ phonemes', rest)

-- XXX change phonemic representation, so visarga, anusvara are annotations on
-- vowels, and we only allow one per vowel.  Thus, [P.Phoneme] can't violate
-- this invariant, and we don't have to worry about extra error conditions.
-- also, does it make sense to conflate the input parsing (devanagari vs
-- non-devanagari unicode) with the translation?  Might be easier to factor for
-- upper levels if we separate the two.  One function splits a string into a
-- list of strings, which alternate: devanagari, not, devanagari, not, ...  (Is
-- there a good typed representation that enforces the alternation?)

-- Conversion; assumes that we've converted at least one phoneme.
toPhonemicMedial :: String -> ([P.Phoneme], String)
toPhonemicMedial [] = ([], [])
toPhonemicMedial s =
  case (T.matchPrefix unicodeTrie s) of
    Just (s', phonemes) ->
      let (phonemes', rest) = toPhonemicMedial s'
      in (phonemes ++ phonemes', rest)
    Nothing -> ([], s)

-- trie for conversion, indexed by unicode
unicodeTrie :: Trie Char [P.Phoneme]
unicodeTrie =
  T.fromList (
    concatMap makeVowelTrieEntries vowelsToPhonemic
    ++ (concatMap makeConsonantTrieEntries consonantsToPhonemic))
    where makeVowelTrieEntry :: (Char, P.Phoneme) -> (String, [P.Phoneme])
          makeVowelTrieEntry (c, p) = ([c], [p])

makeVowelTrieEntries :: (Char, P.Phoneme) -> [(String, [P.Phoneme])]
makeVowelTrieEntries (c, p) =


makeConsonantTrieEntries :: (Char, P.Phoneme) -> [(String, [P.Phoneme])]
makeConsonantTrieEntries (c, p) =
  ([c], [p, P.A])
  : ([c, virama], [p])
  : map (makeEntry c p) [(combAA, P.AA),
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
                         (combAU, P.AU)]
  where makeEntry :: Char -> P.Phoneme -> (Char, P.Phoneme)
                     -> (String, [P.Phoneme])
        makeEntry consonant cPhoneme (vowel, vPhoneme) =
          ([consonant, vowel], [cPhoneme, vPhoneme])

-- maps unicode vowels to phonemic vowels
vowelsToPhonemic :: [(Char, P.Phoneme)]
vowelsToPhonemic =
  [(initA, P.A),
   (initAA, P.AA),
   (combAA, P.AA),
   (initI, P.I),
   (combI, P.I),
   (initII, P.II),
   (combII, P.II),
   (initU, P.U),
   (combU, P.U),
   (initUU, P.UU),
   (combUU, P.UU),
   (initVocR, P.VocR),
   (combVocR, P.VocR),
   (initVocRR, P.VocRR),
   (combVocRR, P.VocRR),
   (initVocL, P.VocL),
   (combVocL, P.VocL),
   (initVocLL, P.VocLL),
   (combVocLL, P.VocLL),
   (initE, P.E),
   (combE, P.E),
   (initAI, P.AI),
   (combAI, P.AI),
   (initO, P.O),
   (combO, P.O),
   (initAU, P.AU),
   (combAU, P.AU)]

-- maps unicode consonsants to simplest phonemic equivalent (discards -a)
consonantsToPhonemic :: [(Char, P.Phoneme)]
consonantsToPhonemic = [(ka, P.K),
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
                        (ha, P.H)]

-- can't just parameterize main fromPhonemic loop over the map, since we need
-- to handle viramas differently.

fromPhonemic :: [P.Phoneme] -> Maybe String
fromPhonemic ps = fromPhonemicWithMap ps afterVowelMap

-- assumes that P.vowels, P.consonants in domain of pmap
fromPhonemicWithMap :: [P.Phoneme] -> Map P.Phoneme String -> Maybe String
fromPhonemicWithMap [] _ = Just []
fromPhonemicWithMap [p] pmap
  | p `S.member` P.consonants = Just (pmap M.! p ++ [virama])
  | otherwise = M.lookup p pmap
fromPhonemicWithMap (p:ps) pmap =
  do u <- M.lookup p pmap
     us <- fromPhonemicWithMap ps (nextState p)
     return (u ++ us)

nextState :: P.Phoneme -> Map P.Phoneme String
nextState P.Visarga = afterVowelMap
nextState P.Anusvara = afterVowelMap
nextState p
  | p `S.member` P.vowels = afterVowelMap
  | p `S.member` P.consonants = afterConsonantMap
  | otherwise = M.empty

initVowelMap :: Map P.Phoneme String
initVowelMap =
  M.fromAscList [(P.A, [initA]),
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

combiningVowelMap :: Map P.Phoneme String
combiningVowelMap =
  M.fromAscList [(P.A, ""),
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

-- map from phonemes to most straightforward (single char) unicode equivalent.
-- That is, maps P.Kha to unicode for syllable /kha/, no virama or combining
-- vowels.
consonantMap :: Map P.Phoneme String
consonantMap =
  M.fromAscList (map (\(x,y) -> (y,[x])) consonantsToPhonemic)

-- map used to translate phonemes that appear initially or after a vowel.  Bare
-- consonants (no leading virama) and initial vowel forms (for hiatus, which is
-- rare but possible), plus visarga.
afterVowelMap :: Map P.Phoneme String
afterVowelMap =
  M.insert P.Visarga [visarga]
  (M.insert P.Anusvara [anusvara]
   (consonantMap `M.union` initVowelMap))

-- map used to translate phonemes that appear after a consonant.  Stick a
-- virama before consonants, and use combining vowel forms.
afterConsonantMap :: Map P.Phoneme String
afterConsonantMap =
  (M.map (virama:) consonantMap) `M.union` combiningVowelMap

mcons :: Maybe a -> Maybe [a] -> Maybe [a]
mcons Nothing _ = Nothing
mcons _ Nothing = Nothing
mcons (Just x) (Just xs) = Just (x : xs)

-}
