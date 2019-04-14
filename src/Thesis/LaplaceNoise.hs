module Thesis.LaplaceNoise where

import System.Random (StdGen, random)

generate :: StdGen -> Double -> (Double, StdGen)
generate gen scale =
  let (rand, newGen) = random gen     -- value in [0, 1)
      flippedUniform = rand - 0.5     -- value in [-0.5, 0.5)
      uniform = negate flippedUniform -- value in (-0.5, 0.5]
      noise = scale * signum uniform * log (1 - 2 * abs uniform)
  in (noise, newGen)
