{-# LANGUAGE
    DuplicateRecordFields
  , RecordWildCards
  , OverloadedStrings
  #-}

module Main where

import Prelude hiding ()
import Control.Arrow
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad.Identity
import Data.Monoid
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T

import Criterion
import Criterion.Main
import Criterion.Measurement 
import Criterion.Internal
import Criterion.Types
import Criterion.Monad
import Criterion.Main.Options (defaultConfig)

import Statistics.Types

import Algebra

sizes :: [Word]
sizes = [2^10, 2^20]

algM = (return :: a -> Identity a) . alg
coAlgM = (return :: a -> Identity a) . coAlg

envs :: [Fix Maybe]
envs = ana coAlg <$> sizes

envsM :: [Identity (Fix Maybe)]
envsM = anaM coAlgM <$> sizes

-- | result best be NFData, measure a field of Measure.
data RoutineGroup parameter result = RoutineGroup
    { _name :: Text
    , _etalon :: parameter -> result
    , _useEnv :: Bool
    , _variants :: [(Text, parameter -> result)]
    , _parameters :: Either [parameter] [(Text, parameter)]
    }

-- | After running it, we shall obtain:
data RoutineStats = RoutineStats
    { _name :: Text
    , _grounds :: [SampleAnalysis]
    , _labels :: [[Text]]
    , _variations :: [[SampleAnalysis]]
    }

data NormalizedRoutineStats = NormalizedRoutineStats
    { _name :: Text
    , _variations :: [[DataPoint]]
    }

data DataPoint = DataPoint { _name :: Text
                           , _mean :: Either Double Double
                           , _variance :: Double
                           } deriving (Show, Eq, Ord)

-- Then we will normalize it with order of speedup = lg D - lg d if D - d positive, or order of
-- slowdown = lg d - lg D if D - d negative.

sequence2 :: (Traversable s, Traversable t, Monad m) => t (s (m a)) -> m (t (s a))
sequence2 = sequence . fmap sequence

fmap2 :: (Functor f, Functor g) => (a -> b) -> g (f a) -> g (f b)
fmap2 = fmap . fmap

zipWith2 :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipWith2 f = zipWith (zipWith f)

zipWith23 :: (a -> b -> c -> d) -> [[a]] -> [[b]] -> [[c]] -> [[d]]
zipWith23 f = zipWith3 (zipWith3 f)

runSingleBench :: Benchmarkable -> IO SampleAnalysis
runSingleBench mark = fmap extractAnalysis
                    $ withConfig defaultConfig
                    $ runAndAnalyseOne errNumber errName mark

  where errNumber = error "This benchmarkable is not numbered."
        errName   = error "This benchmarkable is not named."

        extractAnalysis :: DataRecord -> SampleAnalysis
        extractAnalysis (Analysed report) = reportAnalysis report
        extractAnalysis _ = error $ "exractAnalysis: Got a report that's not analyzed! Panic."


runShowableRoutineGroup :: (Show parameter, NFData parameter, NFData result)
                        => RoutineGroup parameter result -> IO RoutineStats
runShowableRoutineGroup RoutineGroup { .. } = do

    let normalizedParameters = case _parameters of
            Left xs -> (pack . show &&& id) <$> xs
            Right pairs -> pairs
    parameters <- if _useEnv  -- These guys here are evaluated to normal form, and with names.
        then sequence2 . fmap2 (evaluate . force) $ normalizedParameters
        else return normalizedParameters

    let masterBench = nf _etalon <$> (snd <$> parameters)
    masterReport <- sequence . fmap runSingleBench $ masterBench

    let benchmarks = [ [ nf f x | f <- snd <$> _variants ] | x <- snd <$> parameters ]
        labels     = [ [ p <> n | n <- fst <$> _variants ] | p <- fst <$> parameters ]
            -- Every sublist here corresponds to a single entry in masterBench, with parameter as
            -- key. If you imagine the sublists as lines, then this is a matrix and the
            -- masterBench is a column vector.
    reports <- sequence2 . fmap2 runSingleBench $ benchmarks

    return $ RoutineStats _name masterReport labels reports

normalizeRoutineStats :: RoutineStats -> NormalizedRoutineStats
normalizeRoutineStats RoutineStats { .. } = 
    let normalizedVariations = zipWith23 sampleToPoint _labels (repeat <$> _grounds) _variations
        labelledVariations = undefined
    in NormalizedRoutineStats _name normalizedVariations

  where
    sampleToPoint :: Text -> SampleAnalysis -> SampleAnalysis -> DataPoint
    sampleToPoint name
                  SampleAnalysis { anMean   = Estimate { estPoint = gmean }
                                 , anStdDev = Estimate { estPoint = gdev  } }
                  SampleAnalysis { anMean   = Estimate { estPoint = smean }
                                 , anStdDev = Estimate { estPoint = sdev  } }
                  = DataPoint name (normalizeMean gmean smean) (normalizeDeviation gdev sdev)

    normalizeMean gmean smean | gmean < smean = Right $ logBase 2 (smean / gmean)
                                -- ^ The variant is faster than the base. This is right!
                              | gmean > smean = Left  $ logBase 2 (gmean / smean)
                                -- ^ The variant is slower than the base. This is wrong...
                              | gmean == smean = Right 1.0

    normalizeDeviation gdev sdev = sdev / gdev

c = cata alg
fc = fixcata alg
cM = cataM algM
fcM = fixcataM algM

a = ana coAlg
fa = fixana coAlg
aM = anaM coAlgM
faM = fixanaM coAlgM

main = undefined

-- main = defaultMain
--     [ bgroup "cata"
--         [ bgroup "short input"
--             [ env (return shortEnv) $ \x -> bench "cata"    (benchCata x)
--             , env (return shortEnv) $ \x -> bench "fixcata" (benchFixcata x)
--             ]
--         , bgroup "long input"
--             [ env (return longEnv) $ \x -> bench "cata"    (benchCata x)
--             , env (return longEnv) $ \x -> bench "fixcata" (benchFixcata x)
--             ]
--         ]
--     , bgroup "ana"
--         [ bgroup "small word"
--             [ bench "ana" $ benchAna smallWord
--             , bench "fixana" $ benchFixana smallWord
--             ]
--         , bgroup "large word"
--             [ bench "ana" $ benchAna largeWord
--             , bench "fixana" $ benchFixana largeWord
--             ]
--         ]
--     , bgroup "cataM"
--         [ bgroup "short input"
--             [ env (return shortEnv) $ \x -> bench "cataM"    (benchCata x)
--             , env (return shortEnv) $ \x -> bench "fixcataM" (benchFixcata x)
--             ]
--         , bgroup "long input"
--             [ env (return longEnv) $ \x -> bench "cataM"    (benchCata x)
--             , env (return longEnv) $ \x -> bench "fixcataM" (benchFixcata x)
--             ]
--         ]
--     , bgroup "anaM"
--         [ bgroup "small word"
--             [ bench "anaM" $ benchAnaM smallWord
--             , bench "fixanaM" $ benchFixanaM smallWord
--             ]
--         , bgroup "large word"
--             [ bench "anaM" $ benchAnaM largeWord
--             , bench "fixanaM" $ benchFixanaM largeWord
--             ]
--         ]
--     ]
