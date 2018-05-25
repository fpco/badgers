module Handler.Auth.Forms where

import Import

import Helpers.Forms

-- <input name="utf8" type="hidden" value="✓">
-- <input name="authenticity_token" type="hidden" value="N7n9C25j7IjsaolcmEWfTPIMEI/9xif+x4pYSJeuYTMSGnYyRRnGumalT7dSnjF7PqGEHQn/GXWePnRiXBagGA==">
-- <p>
--   <label for="email">
--     E-mail or Username:
--   <input autofocus="autofocus" #email name="email" size="30" type="text" value="">
--   <br>
--   <label for="password">
--     Password:
--   <input #password name="password" size="30" type="password" value="">
--   <br>
-- <p>
--   <input data-disable-with="Login" name="commit" type="submit" value="Login">

loginForm :: Form (Text, Text)
loginForm =
  renderDivs $
  (,) <$> areq textField (named "email" (placeheld "Email")) Nothing
      <*> areq passwordField (named "password" (placeheld "Password")) Nothing


renderLoginForm :: Maybe Text -> Widget
renderLoginForm maybeEmail = do
  let attrs :: [(Text, Text)]
      attrs = [("value", fromMaybe "" maybeEmail)]
  [whamlet|
<input name="utf8" type="hidden" value="✓">
<input name="authenticity_token" type="hidden" value="N7n9C25j7IjsaolcmEWfTPIMEI/9xif+x4pYSJeuYTMSGnYyRRnGumalT7dSnjF7PqGEHQn/GXWePnRiXBagGA==">
<p>
  <label for="email">
    E-mail or Username:
  <input #email
   autofocus="autofocus"
   name="email"
   size="30"
   type="text"
   *{attrs}>
  <br>
  <label for="password">
    Password:
  <input #password name="password" size="30" type="password" value="">
  <br>
|]
