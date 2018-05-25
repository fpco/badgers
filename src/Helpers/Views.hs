module Helpers.Views where

import Import

baseLayout :: Maybe (Entity User)
           -> WidgetT App IO ()
           -> HandlerT App IO Html
baseLayout _ content =
  defaultLayout $ do
    addStylesheet (StaticR css_app_css)
    [whamlet|
<div #wrapper>
  <div #header>
    <div #headerleft>
      <a href="/" #l_holder style="background-color: #ff0000;" title="Lobsters (Current traffic: 99)">
      <span .headerlinks>
        <a href="/">
          Home
        <a href="/recent">
          Recent
        <a href="/comments">
          Comments
        <a href="/search">
          Search
    <div #headerright>
      <span .headerlinks>
        <a href="/login">
          Login
    <div .clear>
  ^{content}
  <div #footer>
    <a href="/moderations">
      Moderation Log
    <a href="/hats">
      Hats
    <a href="https://github.com/lobsters/lobsters/wiki">
      Wiki
    <a href="/privacy">
      Privacy
    <a href="/about">
      About
  <div .clear>

|]
