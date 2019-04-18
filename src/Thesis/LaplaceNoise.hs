-- | Generates Laplace noise
module Thesis.LaplaceNoise where

import System.Random (StdGen, random)

-- | Generates a random variable drawn from a Laplace distribution with the given scale using the
-- given random number generator.
generate :: StdGen -> Double -> (Double, StdGen)
generate gen scale =
  let (rand, newGen) = random gen     -- value in [0, 1)
      flippedUniform = rand - 0.5     -- value in [-0.5, 0.5)
      uniform = negate flippedUniform -- value in (-0.5, 0.5]
      noise = uniformToLaplace scale 0 uniform
  in (noise, newGen)

-- | Transforms a random variable drawn from a uniform distribution in (-0.5, 0.5] into a variable
-- with distribution Laplace(mean, scale). This function contains no checks on the input variable.
uniformToLaplace :: Double -- ^ The scale of the Laplace distribution
                 -> Double -- ^ The mean of the Laplace distribution
                 -> Double -- ^ A random variable drawn uniformly from (-0.5, 0.5]
                 -> Double -- ^ A random value drawn from Laplace(mean, scale)
uniformToLaplace scale mean uniform = mean - scale * signum uniform * log(1 - 2 * abs uniform)
