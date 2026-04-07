-- | Main game module — Miso component with Model, Action, view, and update.
--
-- PURPOSE: Define the top-level Miso application component that drives
-- physics simulation, camera tracking, and canvas rendering.
-- SCOPE: Game Model and Action types, Miso view/update cycle,
-- canvas drawing helpers (drawScene, drawEntity).
module Game where

import Apecs
import Apecs.Physics (Position (..), V2 (..), stepPhysics)
import Data.String (fromString)
import Game.Camera (Camera, follow, withCamera, camWidth, camHeight, isVisible)
import Lib.ECS
import Miso
import Miso.Canvas qualified as C
import Miso.Html
import Miso.Html.Property
import Miso.Lens
import Miso.Prelude

-- | Top-level application state for the Miso game component.
data Model = M
    { modelWorld  :: World   -- ^ Apecs ECS world holding all entities and physics
    , modelStep   :: Int     -- ^ Monotonic tick counter
    , modelCamera :: Camera  -- ^ Current 2D camera state
    }

-- | Lens into the tick counter field.
sL :: Lens Model Int
sL = lens modelStep (\m s' -> m{modelStep = s'})

-- | Lens into the camera field.
camL :: Lens Model Camera
camL = lens modelCamera (\m c -> m{modelCamera = c})

-- | Actions dispatched by the Miso event loop.
data Action
    = Tick                        -- ^ Advance simulation by one frame (16 ms)
    | ApplyCamera Camera          -- ^ Replace the current camera state

-- | Structural equality for Miso virtual DOM diffing, ignoring the ECS 'World'.
--
-- 'World' has no 'Eq' instance, but this is safe because:
--
-- * The step counter @s@ is incremented on every 'Tick' (see 'update'),
--   so the model is /always/ considered changed from one frame to the next.
-- * The canvas draw callback ('drawScene') re-reads ECS state from the
--   'World' on every invocation, so it never relies on a stale snapshot.
instance Eq Model where
    M _ s1 c1 == M _ s2 c2 = s1 == s2 && c1 == c2

-- | Render the current game state as an HTML 'canvas' element.
-- PRE-CONTRACT: The 'World' must have been initialised via 'Lib.ECS'.
-- POST-CONTRACT: Returns a single @<canvas>@ inside a @<div>@ that
-- paints all positioned entities each frame.
view :: Model -> View Model Action
view (M w _ c) =
    div_ []
      [ C.canvas
          [ width_  (fromString (show (round (camWidth  c) :: Int)))
          , height_ (fromString (show (round (camHeight c) :: Int)))
          ]
          (const $ pure ())
          (drawScene c w)
      ]

-- | Process a single 'Action' inside the Miso update loop.
--
-- * @Tick@ steps the physics simulation (16 ms), collects entity positions,
--   and emits 'ApplyCamera' to smoothly track the first entity found.
--   Increments the step counter.
-- * @ApplyCamera newCam@ replaces the stored camera.
--
-- PRE-CONTRACT: 'Tick' requires the ECS world to contain at least the
-- physics component store.
-- POST-CONTRACT: After 'Tick' the step counter is incremented by one and
-- the physics world has advanced by 16 ms.
update :: Action -> Effect parent Model Action
update Tick = do
    M w _ c <- Miso.Prelude.get
    sync_ $ runWith w $ stepPhysics 0.016
    sync $ do
        positions <- runWith w $ collect (\p@(Position _) -> Just p)
        pure $ case positions of
            (Position (V2 x y) : _) -> ApplyCamera (follow 0.1 (x, y) c)
            _ -> ApplyCamera c
    sL += 1
    pure ()

update (ApplyCamera newCam) = do
    camL .= newCam
    pure ()

-- | Clear the canvas and draw all positioned entities through the camera transform.
-- PRE-CONTRACT: The 'World' must have been initialised.
drawScene :: Camera -> World -> () -> C.Canvas ()
drawScene cam w _ = do
    C.clearRect (0, 0, camWidth cam, camHeight cam)
    withCamera cam $ do
        positions <- liftIO $ runWith w $ collect (\p@(Position _) -> Just p)
        let visible = filter (isVisible cam . entityBounds) positions
        mapM_ drawEntity visible

-- | Compute the AABB of an entity (circle of radius 20) for visibility culling.
entityBounds :: Position -> (Double, Double, Double, Double)
entityBounds (Position (V2 x y)) = (x - 20, y - 20, 40, 40)

-- | Draw a single entity as a stroked circle of radius 20 at its world position.
drawEntity :: Position -> C.Canvas ()
drawEntity (Position (V2 x y)) = do
    C.beginPath ()
    C.arc (x, y, 20, 0, 2 * pi)
    C.stroke ()
