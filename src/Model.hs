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
module Model
  ( module Model
  , module Export
  ) where

import ClassyPrelude.Yesod hiding ((==.), hash, on, selectFirst)

import Control.Monad.Logger hiding (LoggingT, runLoggingT)
import Database.Esqueleto hiding (selectFirst)
import Database.Esqueleto.Internal.Sql
import Database.Persist.Postgresql (ConnectionString, withPostgresqlPool)
import Model.BCrypt as Export
import Model.Types as Export
--     t.index ["mailing_list_mode"], name: "mailing_list_enabled"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User sql=users
  username Text sqltype=varchar(50)
  email Text sqltype=varchar(100)
  passwordDigest BCrypt
  createdAt UTCTime
  isAdmin Bool default=FALSE
  passwordResetToken Text Maybe sqltype=varchar(75)
  sessionToken Text Maybe sqltype=varchar(75)
  about Text Maybe sqltype=varchar(10485760)
  invitedByUserId UserId Maybe
  isModerator Bool default=FALSE
  pushoverMentions Bool default=FALSE
  rssToken Text Maybe sqltype=varchar(75)
  mailingListToken Text Maybe sqltype=varchar(75)
  mailingListMode Int64 default=0
  karma Int64 default=0
  bannedAt UTCTime Maybe
  bannedByUserId UserId Maybe
  bannedReason Text Maybe sqltype=varchar(75)
  deletedAt UTCTime Maybe
  disabledInviteAt UTCTime Maybe
  disabledInviteByUserId UserId Maybe
  disabledInviteReason Text Maybe sqltype=varchar(75)
  settings Text

  UniqueUserUsername username
  UniqueMailingListToken mailingListToken !force
  UniquePasswordResetToken passwordResetToken !force
  UniqueRssToken rssToken !force
  UniqueSessionToken sessionToken !force
  deriving Eq Show

-- 10485760 was previously 16777215
Story sql=stories
  createdAt UTCTime
  userId UserId
  url Text sqltype=varchar(250)
  title Text sqltype=varchar(150)
  description Text sqltype=varchar(10485760)
  shortId Text sqltype=varchar(6)
  isExpired Bool default=FALSE
  upvotes Int64 default=0
  downvotes Int64 default=0
  isModerated Bool default=FALSE
  hotness Hotness
  markeddownDescription Text sqltype=varchar(10485760)
  storyCache Text sqltype=varchar(10485760)
  commentsCount Int64 default=0
  mergedStoryId StoryId Maybe
  unavailableAt UTCTime Maybe
  twitterId Text sqltype=varchar(20)
  userIsAuthor Bool default=FALSE
  UniqueShortId shortId
  deriving Eq Ord Show

Tag sql=tags
  tag Text sqltype=varchar(25)
  description Text Maybe sqltype=varchar(100)
  privileged Bool default=FALSE
  isMedia Bool default=FALSE
  inactive Bool default=FALSE
  hotnessMod Double default=0.0
  UniqueTagName tag
  deriving Show

Tagging sql=taggings
  story StoryId
  tag TagId
  UniqueStoryTagPair story tag
  deriving Show

Vote sql=votes
  user UserId
  story StoryId
  vote Int64
  reason Text -- enum?
  updatedAt UTCTime
  deriving Show
|]
--   comment CommentId Maybe

--     t.index ["comment_id"], name: "index_votes_on_comment_id"
--     t.index ["user_id", "comment_id"], name: "user_id_comment_id"
--     t.index ["user_id", "story_id"], name: "user_id_story_id"

-- UniqueCommentVote comment
-- UniqueUserCommentVote user comment
-- UniqueUserStoryVote user story

selectFirst :: ( SqlSelect a r
               , MonadIO m
               )
            => SqlQuery a
            -> SqlReadT m (Maybe r)
selectFirst query = do
  res <- select query
  case res of
    (x : _) -> return (Just x)
    _ -> return Nothing

getUserByEmail :: Text -> DB (Maybe (Entity User))
getUserByEmail email =
  getUserBy UserEmail email

getUserByUsername :: Text -> DB (Maybe (Entity User))
getUserByUsername username =
  getUserBy UserUsername username

getUserBy :: (PersistField a)
          => EntityField User a
          -> a
          -> DB (Maybe (Entity User))
getUserBy field value =
  selectFirst $
  from $ \user -> do
  where_ (user ^. field ==. val value)
  return user

defaultCreateUser :: Text
                  -> Text
                  -> BCrypt
                  -> Maybe UserId
                  -> IO User
defaultCreateUser
  userUsername userEmail
  userPasswordDigest userInvitedByUserId = do
  t <- getCurrentTime
  let userCreatedAt = t
      userIsAdmin = False
      userPasswordResetToken = Nothing
      userSessionToken = Nothing
      userAbout = Nothing
      userIsModerator = False
      userPushoverMentions = False
      userRssToken = Nothing
      userMailingListToken = Nothing
      userMailingListMode = 0
      userKarma = 0
      userBannedAt = Nothing
      userBannedByUserId = Nothing
      userBannedReason = Nothing
      userDeletedAt = Nothing
      userDisabledInviteAt = Nothing
      userDisabledInviteByUserId = Nothing
      userDisabledInviteReason = Nothing
      userSettings = ""
  return $ User{..}

createUser :: Text -> Text -> Text -> Bool -> DB (Entity User)
createUser email pass username _ = do
  hash <- liftIO $ hashPassword pass
  newUser <- liftIO $ defaultCreateUser username email hash Nothing
  userId <- insert newUser
  return (Entity userId newUser)

dumpMigration :: DB ()
dumpMigration = printMigration migrateAll

runMigrations :: DB ()
runMigrations = runMigration migrateAll

devConn :: ConnectionString
devConn =
  "dbname=badgers_dev host=localhost user=postgres password=password port=5432"

runDevDB :: DB a -> IO a
runDevDB a =
  runNoLoggingT $
    withPostgresqlPool devConn 3
      $ \pool -> liftIO $ runSqlPersistMPool a pool

runDevDBV :: DB a -> IO a
runDevDBV a =
  runStdoutLoggingT $
    withPostgresqlPool devConn 3
      $ \pool -> liftIO $ runSqlPersistMPool a pool
