module Handler.Admin where

import Import

import Helpers.Handlers
import Helpers.Views

getAdminR :: UserId -> Handler Html
getAdminR userId = do
  (Entity _ (User{..})) <-
    runDBOr404 (getUserEntityFromId userId)
  let isOrIsNot :: Text
      isOrIsNot =
        if userAdmin
        then "IS"
        else "IS NOT"
      header =
        [st|#{userEmail} #{isOrIsNot} an Admin!|]
  baseLayout Nothing $ do
    setTitle "Home"
    [whamlet|
    <h1>#{header}
    <form method="POST" action="@{AdminR userId}">
      <input .button type="submit" value="Ascend">
    |]

postAdminR :: UserId -> Handler Html
postAdminR userId = do
  runDB $ update userId [UserAdmin =. True]
  redirect $ AdminR userId
