module Example.LSystem where

import Prelude

import Data.Maybe
import Data.Array (concatMap, foldM)

import Control.Monad.Eff

import Graphics.Canvas hiding (translate)

lsystem :: forall a m s. (Monad m) =>
                         Array a ->
                         (a -> Array a) ->
                         (s -> a -> m s) ->
                         Int ->
                         s -> m s
lsystem init prod interpret n state = go init n
  where
  go s 0 = foldM interpret state s
  go s n = go (concatMap prod s) (n - 1)

data Alphabet = L | R | F

type Sentence = Array Alphabet

type State =
  { x :: Number
  , y :: Number
  , theta :: Number
  }

main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  let
    initial :: Sentence
    initial = [F, R, R, F, R, R, F, R, R]

    productions :: Alphabet -> Sentence
    productions L = [L]
    productions R = [R]
    productions F = [F, L, F, R, R, F, L, F]

    interpret :: State -> Alphabet -> Eff (canvas :: Canvas) State
    interpret state L = return $ state { theta = state.theta - Math.pi / 3.0 }
    interpret state R = return $ state { theta = state.theta + Math.pi / 3.0 }
    interpret state F = do
      let x' = state.x + Math.cos state.theta * 6.0
          y' = state.y + Math.sin state.theta * 6.0
      lineTo ctx x' y'
      return { x: x', y: y', theta: state.theta }

    initialState :: State
    initialState = { x: 60.0, y: 160.0, theta: 0.0 }

  setStrokeStyle "#000000" ctx

  fillPath ctx $ do
    moveTo ctx initialState.x initialState.y
    lsystem initial productions interpret 4 initialState
    closePath ctx


-- Exercises
-- (Easy) Modify the L-system example above to use fillPath instead of strokePath. Hint: you will need to include a call to closePath, and move the call to moveTo outside of the interpret function.
-- done!

-- (Easy) Try changing the various numerical constants in the code, to understand their effect on the rendered system.
-- done!

-- (Medium) Break the lsystem function into two smaller functions. The first should build the final sentence using repeated application of concatMap, and the second should use foldM to interpret the result.

-- (Medium) Add a drop shadow to the filled shape, by using the setShadowOffsetX, setShadowOffsetY, setShadowBlur and setShadowColor actions. Hint: use PSCi to find the types of these functions.

-- (Medium) The angle of the corners is currently a constant (pi/3). Instead, it can be moved into the Alphabet data type, which allows it to be changed by the production rules:
{--
type Angle = Number

data Alphabet = L Angle | R Angle | F
--}
-- How can this new information be used in the production rules to create interesting shapes?

-- (Difficult) An L-system is given by an alphabet with four letters: L (turn left through 60 degrees), R (turn right through 60 degrees), F (move forward) and M (also move forward).
-- The initial sentence of the system is the single letter M.
-- The production rules are specified as follows:
{--
L -> L
R -> R
F -> FLMLFRMRFRMRFLMLF
M -> MRFRMLFLMLFLMRFRM
--}
-- Render this L-system. Note: you will need to decrease the number of iterations of the production rules, since the size of the final sentence grows exponentially with the number of iterations.

-- Now, notice the symmetry between L and M in the production rules. The two “move forward” instructions can be differentiated using a Boolean value using the following alphabet type:
{--
data Alphabet = L | R | F Boolean
--}
-- Implement this L-system again using this representation of the alphabet.

-- (Difficult) Use a different monad m in the interpretation function. You might try using the CONSOLE effect to write the L-system onto the console, or using the RANDOM effect to apply random “mutations” to the state type.
