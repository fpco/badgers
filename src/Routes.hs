module Routes where

import Import.NoFoundation

import AppType

mkYesodData "App" [parseRoutes|
/static StaticR Static appStatic

/ HomeR GET

-- Users
/u/#UserId/admin AdminR GET POST
/settings SettingsR GET POST

|]
-- /u/#Text UserR GET
-- Auth
-- /login      LoginR   GET POST
-- /signup     SignupR  GET POST
-- /signout    SignoutR GET
