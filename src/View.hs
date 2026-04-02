module View where

import Lib.Recipe (Recipe)
import Miso
import Miso.Html
import Miso.Prelude

type Action = ()
type Model = Recipe

updateModel :: Action -> Effect parent Model Action
updateModel _ = pure ()

viewModel :: Model -> View Model Action
viewModel _ = div_ [] [text "Hello, Misoooooo!"]