module Game where

import Apecs
import Apecs.Physics (Position (..), V2 (..), stepPhysics)
import Lib.ECS
import Miso
import Miso.Canvas qualified as C
import Miso.Html
import Miso.Html.Property
import Miso.Lens
import Miso.Prelude

data Model = M {w :: World, s :: Int}

sL :: Lens Model Int
sL = lens s (\m s' -> m{s = s'})

data Action = Tick

instance Eq Model where
    M _ s1 == M _ s2 = s1 == s2

view :: Model -> View Model Action
view (M w _) = div_ [] [C.canvas [width_ "400", height_ "400"] (const $ pure ()) (drawCircle w)]

update :: Action -> Effect parent Model Action
update Tick = do
    M w _ <- Miso.Prelude.get
    sync_ $ runWith w $ do
        -- Step the physics simulation
        stepPhysics 0.016
    sL += 1
    pure ()

drawCircle :: World -> () -> C.Canvas ()
drawCircle w _ = do
    [Position (V2 x y)] <- runWith w $ collect (\p@(Position _) -> Just p)
    liftIO $ putStrLn $ "Drawing circle at: " ++ show (x, y)
    C.beginPath ()
    C.arc (x, y, 20, 0, 2 * pi)
    C.stroke ()