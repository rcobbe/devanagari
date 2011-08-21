module Text.Devanagari.Phonemic(Phoneme(..))
where

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
               deriving (Eq, Show, Ord)

