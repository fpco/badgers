module Helpers.Views where

import Import

baseLayout :: Widget -> Handler Html
baseLayout content =
  defaultLayout $ do
    setTitle "Blah"
    [whamlet|
^{content}
|]
