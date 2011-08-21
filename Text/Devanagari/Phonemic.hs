module Text.Devanagari.Phonemic(Phoneme(..), vowels, consonants)
where

import Data.Set (Set)
import qualified Data.Set as S

data Phoneme = A
             | AA
             | I
             | II
             | U
             | UU
             | VocR             -- short vocalic R
             | VocRR            -- long vocalic R
             | VocL             -- short vocalic L
             | VocLL            -- long vocalic L
             | E
             | AI
             | O
             | AU
             | Visarga
             | Anusvara
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
               deriving (Eq, Show, Ord, Enum)

vowels :: Set Phoneme
vowels = S.fromList [A .. AU]

consonants :: Set Phoneme
consonants = S.fromList [K .. H]
