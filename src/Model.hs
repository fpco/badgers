{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model where

import ClassyPrelude.Yesod

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    ident Text
    password Text Maybe
    UniqueUser ident
Email
    email Text
    userId UserId Maybe
    verkey Text Maybe
    UniqueEmail email
Comment json -- Adding "json" causes ToJSON and FromJSON instances to be derived.
    message Text
    userId UserId Maybe
    deriving Eq
    deriving Show
|]
