module Handler.Settings where

import Import

import Handler.Auth
import Handler.Sessions
import Helpers.Forms
import Helpers.Views

-- QUESTION: What's the best method for including the extant About data in the form?
settingsForm :: Form Text
settingsForm =
  renderDivs $ areq textField (named "about" (placeheld "About")) Nothing

renderSettings :: Widget -> Handler Html
renderSettings widget = do
  baseLayout Nothing $ do
    setTitle "Settings"
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
  ((result, widget), _) <- runFormPost settingsForm
  case result of
    FormSuccess about -> do
      runDB $ update userId [UserAbout =. Just about]
      renderSettings widget
    _ -> renderSettings widget

