module Handler.Home where

import Import

import Helpers.Views

getHomeR :: Handler Html
getHomeR =
  baseLayout Nothing $ do
    setTitle "Home"
    [whamlet|
<h1>Hello World
|]
