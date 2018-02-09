
module Main where

import Criterion
import Criterion.Main
import Criterion.Measurement 

import Algebra
import Filesystem

benchAna = nf (ana coalg1) (1000 :: Word)
benchFixana = nf (fixana coalg1) (1000 :: Word)

longBenchAna = nf (ana coalg1) (2^20 :: Word)
longBenchFixana = nf (fixana coalg1) (2^20 :: Word)

main = defaultMain
    [ bgroup "anamorphisms"
        [ bench "ana" $ benchAna
        , bench "fixana" $ benchFixana
        ]
    , bgroup "anamorphisms with long chains"
        [ bench "ana" $ longBenchAna
        , bench "fixana" $ longBenchFixana
        ]
    ]

