module Text.Devanagari.Segments(Segment(..), VowelMod(..), isVowel)
where

-- XXX discuss rationale for allowing only a single modifier on any given
-- vowel.  Simplifies representation, and satisfies all known use cases.

-- | Segmental representation of Sanskrit, in standard lexicographic ordering.
-- We represent visarga and anusvaras as modifiers on vowels, since they may
-- only appear immediately after vowels.  (Note that in the standard
-- lexicographic ordering, visarga and anusvara appear in that order
-- between vowels and consonants, so this representation change preserves the
-- standard lexicographical ordering on [Segment], except in cases involving
-- vowel hiatus: [T, E Visarga, A NoMod, P, I] > [T, E NoMod, A NoMod, P, I],
-- even though the normal Sanskrit ordering would be otherwise.
data Segment = A { modifier :: VowelMod }
             | AA { modifier :: VowelMod }
             | I { modifier :: VowelMod }
             | II { modifier :: VowelMod }
             | U { modifier :: VowelMod }
             | UU { modifier :: VowelMod }
             | VocR { modifier :: VowelMod }  -- short vocalic R
             | VocRR { modifier :: VowelMod } -- long vocalic R
             | VocL { modifier :: VowelMod }  -- short vocalic L
             | VocLL { modifier :: VowelMod } -- long vocalic L
             | E { modifier :: VowelMod }
             | AI { modifier :: VowelMod }
             | O { modifier :: VowelMod }
             | AU { modifier :: VowelMod }
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

-- XXX why NoMod instead of Maybe?  convenient for parsing, but is that
-- sufficient justification?
data VowelMod = NoMod | Visarga | Anusvara
              deriving (Eq, Show, Ord)

isVowel :: Segment -> Bool
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
