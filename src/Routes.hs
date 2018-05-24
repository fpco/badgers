module Routes where

import Import.NoFoundation

import AppType

mkYesodData "App" [parseRoutes|
/static StaticR Static appStatic

/ HomeR GET

-- Users
/users/#UserId/admin AdminR GET POST

-- Auth
/login      LoginR   GET POST
/signup     SignupR  GET POST
/signout    SignoutR GET
|]
