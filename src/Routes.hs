module Routes where

import Import.NoFoundation

import AppType

mkYesodData "App" [parseRoutes|
/static StaticR Static appStatic

/ HomeR GET

-- Users
/u/#UserUsername/admin AdminR GET POST
/u/#UserUsername UserR GET
/settings SettingsR GET POST

-- Auth
/login      LoginR   GET POST
/signup     SignupR  GET POST
/signout    SignoutR GET
|]
