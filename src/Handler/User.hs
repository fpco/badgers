module Handler.User where

import Import

import Helpers.Views

getUserR :: UserUsername -> Handler Html
getAdminR (UserUsername username) = do
  maybeUser <- runDB (getUserEntityFromUsername username)
  case maybeUser of
    Just (Entity _ user) ->
      baseLayout Nothing $ do
        setTitle "Home"
        [whamlet|
        <h1>#{userUsername user}
        <p>#{userAbout user}
        |]
    Nothing ->
      baseLayout Nothing $ do
        setTitle "Home"
        [whamlet|
        <h1>INVALID USERNAME
        |]
