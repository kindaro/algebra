
module Main where

import Criterion
import Criterion.Main
import Criterion.Measurement 

import Algebra

smallWord, largeWord :: Word
smallWord = 2^10
largeWord = 2^20

shortEnv, longEnv :: Fix Maybe
shortEnv = ana coAlg smallWord
longEnv = ana coAlg largeWord

benchCata = nf (cata alg)
benchFixcata = nf (fixcata alg)

benchAna = nf (ana coAlg)
benchFixana = nf (fixana coAlg)

longBenchAna = nf (ana coAlg)
longBenchFixana = nf (fixana coAlg)

main = defaultMain
    [ bgroup "cata"
        [ bgroup "short input"
            [ env (return shortEnv) $ \x -> bench "cata"    (benchCata x)
            , env (return shortEnv) $ \x -> bench "fixcata" (benchFixcata x)
            ]
        , bgroup "long input"
            [ env (return longEnv) $ \x -> bench "cata"    (benchCata x)
            , env (return longEnv) $ \x -> bench "fixcata" (benchFixcata x)
            ]
        ]
    , bgroup "ana"
        [ bgroup "small word"
            [ bench "ana" $ benchAna smallWord
            , bench "fixana" $ benchFixana smallWord
            ]
        , bgroup "large word"
            [ bench "ana" $ longBenchAna largeWord
            , bench "fixana" $ longBenchFixana largeWord
            ]
        ]
    ]
