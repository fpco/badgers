module Handler.Admin where

import Import

import Helpers.Views

getAdminR :: UserId -> Handler Html
getAdminR userId = do
  maybeUser <- runDB (getUserEntityFromId userId)
  case maybeUser of
    Just user ->
      baseLayout Nothing $ do
        setTitle "Home"
        [whamlet|
        <h1>GOT #{show user}
        <form method="POST" action="@{AdminR userId}">
          <input .button type="submit" value="Ascend">
        |]
    Nothing ->
      baseLayout Nothing $ do
        setTitle "Home"
        [whamlet|
        <h1>NO USER
        |]

postAdminR :: UserId -> Handler Html
postAdminR userId = do
  runDB $ update userId [UserAdmin =. True]
  redirect $ AdminR userId
