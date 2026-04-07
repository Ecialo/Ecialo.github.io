-- | 2D camera system for canvas-based game rendering.
--
-- PURPOSE: Define a 2D camera with coordinate transforms, visibility culling,
-- canvas transform application, and smooth target following.
-- SCOPE: Camera type, world↔screen coordinate conversion, AABB visibility
-- checks, canvas drawing context setup, and linear-interpolation follow.
module Game.Camera
    ( Camera             -- ^ Opaque 2D camera type; use 'mkCamera' to construct.
    , mkCamera           -- ^ Smart constructor (validates zoom > 0, dims > 0).
    , camX               -- ^ Camera centre X in world coordinates.
    , camY               -- ^ Camera centre Y in world coordinates.
    , camZoom            -- ^ Uniform scale factor.
    , camWidth           -- ^ Viewport width in pixels.
    , camHeight          -- ^ Viewport height in pixels.
    , worldToScreen
    , screenToWorld
    , visibleBounds
    , isVisible
    , withCamera
    , follow
    ) where

import Miso.Canvas qualified as C
import Prelude

-- | A 2D camera parameterised by its centre in world coordinates,
-- a uniform zoom factor, and the viewport dimensions in pixels.
-- Derived instances: 'Show' renders all fields; 'Eq' is structural equality.
data Camera = Camera
    { camX, camY       :: Double
    -- ^ Camera centre in world coordinates
    , camZoom          :: Double
    -- ^ Uniform scale factor (1.0 = 1:1, >1 = zoomed in)
    , camWidth, camHeight :: Double
    -- ^ Viewport dimensions in pixels
    }
    deriving (Show, Eq)

-- | Smart constructor for 'Camera'.  Validates that 'camZoom' is strictly
-- positive and that viewport dimensions are strictly positive.
-- Calls 'error' on invalid input (programmer bug).
mkCamera :: Double -> Double -> Double -> Double -> Double -> Camera
mkCamera x y zoom w h
    | zoom <= 0  = error $ "mkCamera: camZoom must be > 0, got " ++ show zoom
    | w <= 0     = error $ "mkCamera: camWidth must be > 0, got " ++ show w
    | h <= 0     = error $ "mkCamera: camHeight must be > 0, got " ++ show h
    | otherwise  = Camera { camX = x, camY = y, camZoom = zoom, camWidth = w, camHeight = h }

-- | Convert world coordinates to screen (pixel) coordinates.
-- PRE-CONTRACT: 'camZoom' must be non-zero.
-- POST-CONTRACT: @screenToWorld cam (worldToScreen cam p) == p@ (round-trip).
worldToScreen :: Camera -> (Double, Double) -> (Double, Double)
worldToScreen cam (wx, wy) =
    ( (wx - camX cam) * camZoom cam + camWidth cam / 2
    , (wy - camY cam) * camZoom cam + camHeight cam / 2
    )

-- | Convert screen (pixel) coordinates to world coordinates.
-- PRE-CONTRACT: 'camZoom' must be non-zero.
-- POST-CONTRACT: @worldToScreen cam (screenToWorld cam p) == p@ (round-trip).
screenToWorld :: Camera -> (Double, Double) -> (Double, Double)
screenToWorld cam (sx, sy) =
    ( (sx - camWidth cam / 2) / camZoom cam + camX cam
    , (sy - camHeight cam / 2) / camZoom cam + camY cam
    )

-- | Compute the axis-aligned bounding box of the visible region in world
-- coordinates.  Returns @(x, y, width, height)@ where @(x, y)@ is the
-- top-left corner.
-- POST-CONTRACT: Width and height are positive when 'camZoom' > 0.
visibleBounds :: Camera -> (Double, Double, Double, Double)
visibleBounds cam =
    let hw = camWidth cam / (2 * camZoom cam)
        hh = camHeight cam / (2 * camZoom cam)
    in (camX cam - hw, camY cam - hh, 2 * hw, 2 * hh)

-- | Check whether an AABB (given as @(x, y, width, height)@ in world
-- coordinates) overlaps the camera's visible region.
-- POST-CONTRACT: Returns 'True' iff the AABB has a non-empty intersection
-- with 'visibleBounds'.
isVisible :: Camera -> (Double, Double, Double, Double) -> Bool
isVisible cam (x, y, w, h) =
    let (vx, vy, vw, vh) = visibleBounds cam
    in x + w > vx && x < vx + vw
    && y + h > vy && y < vy + vh

-- | Apply a camera transform to a Canvas drawing action.
-- Sets up translate→scale→translate so that subsequent draw commands
-- operate in world coordinates.
-- PRE-CONTRACT: Must be called within an active canvas rendering context.
-- POST-CONTRACT: The canvas state (transform, clip, etc.) is restored to
-- its pre-call state after the action completes.
withCamera :: Camera -> C.Canvas a -> C.Canvas a
withCamera cam draw = do
    C.save ()
    C.translate (camWidth cam / 2, camHeight cam / 2)
    C.scale (camZoom cam, camZoom cam)
    C.translate (-camX cam, -camY cam)
    result <- draw
    C.restore ()
    pure result

-- | Smoothly move the camera centre towards a target position using
-- linear interpolation each frame.
-- PRE-CONTRACT: The lerp factor should typically be in [0, 1].
-- POST-CONTRACT: Returns a 'Camera' with 'camX' and 'camY' interpolated
-- between their current values and the target.
follow :: Double -> (Double, Double) -> Camera -> Camera
follow lerpFactor (tx, ty) cam =
    cam { camX = lerp lerpFactor tx (camX cam)
        , camY = lerp lerpFactor ty (camY cam)
        }
  where
    -- | Linear interpolation: moves @current@ towards @target@ by fraction @t@.
    lerp t target current = current + t * (target - current)
