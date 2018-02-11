
module Main where

import Criterion
import Criterion.Main
import Criterion.Measurement 
import Control.Monad.Identity

import Algebra

smallWord, largeWord :: Word
smallWord = 2^10
largeWord = 2^20

shortEnv, longEnv :: Fix Maybe
shortEnv = ana coAlg smallWord
longEnv = ana coAlg largeWord

shortEnvM, longEnvM :: Monad m => m (Fix Maybe)
shortEnvM = anaM (return . coAlg) smallWord
longEnvM = anaM (return . coAlg) largeWord

c = cata alg
fc = fixcata alg
cM = cataM ((return :: a -> Identity a) . alg)
fcM = fixcataM ((return :: a -> Identity a) . alg)

a = ana coAlg
fa = fixana coAlg
aM = anaM ((return :: a -> Identity a) . coAlg)
faM = fixanaM ((return :: a -> Identity a) . coAlg)

benchCata = nf c
benchFixcata = nf fc

benchCataM = nf cM
benchFixcataM = nf fcM

benchAna = nf a
benchFixana = nf fa


benchAnaM = nf aM
benchFixanaM = nf faM

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
            [ bench "ana" $ benchAna largeWord
            , bench "fixana" $ benchFixana largeWord
            ]
        ]
    , bgroup "cataM"
        [ bgroup "short input"
            [ env (return shortEnv) $ \x -> bench "cataM"    (benchCata x)
            , env (return shortEnv) $ \x -> bench "fixcataM" (benchFixcata x)
            ]
        , bgroup "long input"
            [ env (return longEnv) $ \x -> bench "cataM"    (benchCata x)
            , env (return longEnv) $ \x -> bench "fixcataM" (benchFixcata x)
            ]
        ]
    , bgroup "anaM"
        [ bgroup "small word"
            [ bench "anaM" $ benchAnaM smallWord
            , bench "fixanaM" $ benchFixanaM smallWord
            ]
        , bgroup "large word"
            [ bench "anaM" $ benchAnaM largeWord
            , bench "fixanaM" $ benchFixanaM largeWord
            ]
        ]
    ]
