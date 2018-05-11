module Handler.Home where

import Import

import Helpers.Views

getHomeR :: Handler Html
getHomeR =
  baseLayout $ do
    setTitle "Woot"
    [whamlet|
<h1>Hello World
|]
