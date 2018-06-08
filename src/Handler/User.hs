module Handler.User where

import Import

import Helpers.Views

-- getUserR :: Text -> Handler Html
-- getUserR username = do
--   maybeUser <- runDB (getUserByUsername username)
--   case maybeUser of
--     Just (Entity _ user) ->
--       baseLayout Nothing $ do
--         setTitle "Home"
--         [whamlet|
--         <h1>#{userUsername user}
--         <p>#{fromMaybe "" $ userAbout user}
--         |]
--     Nothing ->
--       baseLayout Nothing $ do
--         setTitle "Home"
--         [whamlet|
--         <h1>INVALID USERNAME
--         |]
