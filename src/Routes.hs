module Routes where

import Import.NoFoundation

import AppType

mkYesodData "App" [parseRoutes|
/static StaticR Static appStatic

/ HomeR GET
/s/#Text/#Text CommentR GET

-- Users
/u/#UserId/admin AdminR GET POST
/t/#Text TagR GET
/settings SettingsR GET POST

-- Auth
/login      LoginR   GET POST
/signup     SignupR  GET POST
/signout    SignoutR GET
|]
