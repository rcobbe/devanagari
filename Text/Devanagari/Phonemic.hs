module Text.Devanagari.Phonemic(Phoneme(..), VowelMod(..), isVowel)
where

-- import Data.Set (Set)
-- import qualified Data.Set as S

-- | Phonemic representation of Sanskrit, in standard lexicographic ordering.
-- We represent visarga and anusvaras as modifiers on vowels, since they may
-- only appear immediately after vowels.  (Note that in the standard
-- lexicographic ordering,
data Phoneme = A VowelMod
             | AA VowelMod
             | I VowelMod
             | II VowelMod
             | U VowelMod
             | UU VowelMod
             | VocR VowelMod    -- short vocalic R
             | VocRR VowelMod   -- long vocalic R
             | VocL VowelMod    -- short vocalic L
             | VocLL VowelMod   -- long vocalic L
             | E VowelMod
             | AI VowelMod
             | O VowelMod
             | AU VowelMod
             | K
             | Kh
             | G
             | Gh
             | Ng               -- guttural (velar) nasal
             | C
             | Ch
             | J
             | Jh
             | PalN             -- palatal nasal
             | RetT             -- retroflex (cerebral) consonant
             | RetTh
             | RetD
             | RetDh
             | RetN
             | T
             | Th
             | D
             | Dh
             | N
             | P
             | Ph
             | B
             | Bh
             | M
             | Y
             | R
             | L
             | V
             | PalS             -- palatal sibilant
             | RetS             -- retroflex sibilant
             | S
             | H
               deriving (Eq, Show, Ord)

data VowelMod = NoMod | Visarga | Anusvara
              deriving (Eq, Show, Ord)

isVowel :: Phoneme -> Bool
isVowel (A _) = True
isVowel (AA _) = True
isVowel (I _) = True
isVowel (II _) = True
isVowel (U _) = True
isVowel (UU _) = True
isVowel (VocR _) = True
isVowel (VocRR _) = True
isVowel (VocL _) = True
isVowel (VocLL _) = True
isVowel (E _) = True
isVowel (AI _) = True
isVowel (O _) = True
isVowel (AU _) = True
isVowel _ = False
