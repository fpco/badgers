module Handler.Settings where

import Import

import Handler.Sessions
import Helpers.Forms
import Helpers.Views

-- QUESTION: What's the best method for including the extant About data in the form?
settingsForm :: Form Text
settingsForm =
  renderDivs $ areq textField (named "about" (placeheld "About")) Nothing

requireUser :: Handler (Entity User)
requireUser = do
  maybeUser <- getUser
  case maybeUser of
    Nothing -> redirect LoginR
    (Just user) -> return user

renderSettings :: Widget -> Handler Html
renderSettings widget = do
  baseLayout Nothing $ do
    setTitle "Login"
    [whamlet|
<div .row #content>
  <div .medium-8 .columns>
    <hr>
<div .row #content>
  <div .medium-8 .columns>
    <h3>Update your profile.
    <form method="POST" action="@{SettingsR}">
      ^{widget}
      <input .button type="submit" value="Submit">
|]

getSettingsR :: Handler Html
getSettingsR = do
  (settingsFormWidget, _) <- generateFormPost settingsForm
  renderSettings settingsFormWidget

postSettingsR :: Handler Html
postSettingsR = do
  (Entity userId _) <- requireUser
  ((result, widget), _) <- runFormPost loginForm
  case result of
    FormSuccess about -> do
      runDB $ update userId [UserAbout =. about]
      renderSettings widget
    _ -> renderSettings widget

