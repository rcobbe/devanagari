module Text.Devanagari.Phonemic(Phoneme(..))
where

data Phoneme = AKara
             | AAKara
             | IKara
             | IIKara
             | UKara
             | UUKara
             | VocRKara         -- short vocalic R
             | VocRRKara        -- long vocalic R
             | VocLKara         -- short vocalic L
             | VocLLKara        -- long vocalic L
             | EKara
             | AIKara
             | OKara
             | AUKara
             | Visarga
             | Anusvara
             | KaKara
             | KhaKara
             | GaKara
             | GhaKara
             | NgaKara          -- guttural (velar) nasal
             | CaKara
             | ChaKara
             | JaKara
             | JhaKara
             | PalNaKara        -- palatal nasal
             | RetTaKara        -- retroflex (cerebral) consonant
             | RetThaKara
             | RetDaKara
             | RetDhaKara
             | RetNaKara
             | TaKara
             | ThaKara
             | DaKara
             | DhaKara
             | NaKara
             | PaKara
             | PhaKara
             | BaKara
             | BhaKara
             | MaKara
             | YaKara
             | Repha
             | LaKara
             | VaKara
             | PalSaKara        -- palatal sibilant
             | RetSaKara        -- retroflex sibilant
             | SaKara
             | HaKara
               deriving (Eq, Show, Ord)

