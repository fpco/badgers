module Handler.Auth.Views where

import Import

import Handler.Auth.Forms
import Helpers.Views

renderLogin :: Widget -> Handler Html
renderLogin widget = do
  baseLayout Nothing $ do
    setTitle "Login"
    [whamlet|
<div #inside>
  <div .box .wide>
    <div .legend>
      Login

    <form accept-charset="UTF-8" action="/login" method="post">
      ^{renderLoginForm (Just "chris@lol.com")}
      <p>
        <input data-disable-with="Login" name="commit" type="submit" value="Login">

      <p>
        Forgot your password or deleted your account?

        <a href="/login/forgot_password">
          Reset your password
        .

      <p>
        Not a user yet?  Signup is by invitation only to combat spam and increase
                accountability.  If you know 
        <a href="/u/">
          a current user
        of the site,
                ask them for an invitation or
                  request one in 
        <a href="/chat">
          chat
        .
|]

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

-- <div .row #content>
--   <div .medium-8 .columns>
--     <hr>
-- <div .row #content>
--   <div .medium-8 .columns>
--     <h3>Login to your account!
--     <form method="POST" action="@{LoginR}">
--       ^{widget}
--       <input .button type="submit" value="Submit">
