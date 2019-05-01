module Main where

import Data.Function (($))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Unit (unit)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit)
import Pretty (pretty)
import Run (run)
import Types.Integer (sampleTerm)

main :: Effect Unit
main = log $ pretty sampleTerm <> " = " <> show (run sampleTerm unit)
