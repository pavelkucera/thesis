module Thesis.LaplaceNoise where

import System.Random (StdGen, random)

generate :: StdGen -> Double -> (Double, StdGen)
generate gen scale =
  let (rand, newGen) = random gen     -- value in [0, 1)
      flippedUniform = rand - 0.5     -- value in [-0.5, 0.5)
      uniform = negate flippedUniform -- value in (-0.5, 0.5]
      noise = uniformToLaplace scale 0 uniform
  in (noise, newGen)

-- uniform is assumed to be a random variable drawn from a uniform distribution in (-0.5, 0.5]
uniformToLaplace :: Double -> Double -> Double -> Double
uniformToLaplace scale mean uniform = mean - scale * signum uniform * log(1 - 2 * abs uniform)
