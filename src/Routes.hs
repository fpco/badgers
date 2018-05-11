module Routes where

import Import.NoFoundation

import AppType

mkYesodData "App" [parseRoutes|
/static StaticR Static appStatic

/ HomeR GET
|]
