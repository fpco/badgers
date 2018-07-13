{-# LANGUAGE StrictData #-}

module Model.Fixtures where

import Import

import qualified Test.RandomStrings as RS

import Prelude ((!!))

data UserFixtures =
  UserFixtures { allUsersF :: [Entity User] }
  deriving (Eq, Show)

data StoryFixtures =
  StoryFixtures { allStoriesF :: [Entity Story] }
  deriving (Eq, Show)

data TaggingFixtures =
  TaggingFixtures { allTaggingsF :: [Entity Tagging] }
  deriving (Eq, Show)

data TagFixtures =
  TagFixtures { allTagsF :: [Entity Tag] }
  deriving (Eq, Show)

data Fixtures =
  Fixtures { userF     :: UserFixtures
           , storyF    :: StoryFixtures
           , tagF      :: TagFixtures
           , taggingF  :: TaggingFixtures
           }
  deriving (Eq, Show)

chrisEmail, chrisPassword :: Text
chrisEmail = "chris@lol.com"
chrisPassword = "chrisPass"

alexeyEmail, alexeyPassword :: Text
alexeyEmail = "alexey@lol.com"
alexeyPassword = "alexeyPass"

makeAccount :: Text -> Text -> Text -> Bool -> DB (Entity User)
makeAccount email pass username isAdmin = do
  userEnt <- createUser email pass username isAdmin
  return userEnt

makeAccounts :: DB [Entity User]
makeAccounts =
  sequenceA [ makeAccount chrisEmail chrisPassword "chris" True
            , makeAccount alexeyEmail alexeyPassword "alexey" False ]

defaultCreateStory :: UserId -> Text -> IO Story
defaultCreateStory userId storyTitle = do
  shortId <- RS.randomString (RS.onlyAlpha RS.randomASCII) 6
  let storyCreatedAt = UTCTime (ModifiedJulianDay 10000) 0
      storyUserId = userId
      storyUrl = "http://www.google.com"
      storyDescription = "My story description"
      storyShortId = pack shortId -- "ABC123"
      storyIsExpired = False
      storyUpvotes = 10
      storyDownvotes = 2
      storyIsModerated = False
      storyHotness = 10
      storyMarkeddownDescription = "My story description"
      storyStoryCache = ""
      storyCommentsCount = 4
      storyMergedStoryId = Nothing
      storyUnavailableAt = Nothing
      storyTwitterId = "bitemyapp"
      storyUserIsAuthor = False
  return $ Story{..}

makeStories :: UserId -> DB [Entity Story]
makeStories userId = do
  let storyTitles = [ "Lessons learned Working From Home"
                    , "Static Sites vs CMS"
                    , "First Impressions of the Rust Programming Language"
                    ]
  stories <- liftIO $ traverse (defaultCreateStory userId) storyTitles
  traverse insertEntity stories

defaultCreateTag :: Text -> Tag
defaultCreateTag tagTag =
  let
    tagDescription = Nothing
    tagPrivileged = False
    tagIsMedia = False
    tagInactive = False
    tagHotnessMod = 0
  in
    Tag{..}

makeTags :: DB [Entity Tag]
makeTags = do
  let tagNames = [ "python"
                 , "haskell"
                 , "programming"
                 , "practices"
                 ]
      tags = map defaultCreateTag tagNames
  traverse insertEntity tags


-- [(StoryId, [Entity Tag])]

makeTaggings :: [TagId] -> [StoryId] -> DB [Entity Tagging]
makeTaggings tagIds storyIds =
  let tagsForStories =
        [ [ tagIds !! 0
          , tagIds !! 2
          ]
        , [ tagIds !! 1
          , tagIds !! 3
          ]
        , [ tagIds !! 0
          , tagIds !! 3
          ]
        ]
      defaultTags = repeat tagIds
      storyTagPairs :: [(StoryId, [TagId])]
      storyTagPairs = zip storyIds (tagsForStories <> defaultTags)
      taggingsForSTP :: (StoryId, [TagId])
                     -> [Tagging]
      taggingsForSTP (story, tags) =
        fmap (Tagging story) tags
      taggings :: [Tagging]
      taggings =
        storyTagPairs >>= taggingsForSTP
  in
    traverse insertEntity taggings

insertFixtures :: DB Fixtures
insertFixtures = do
  allUsersF <- makeAccounts
  let userId = entityKey (allUsersF !! 0)
  allStoriesF <- makeStories userId
  allTagsF <- makeTags
  allTaggingsF <-
    makeTaggings
      (fmap entityKey allTagsF)
      (fmap entityKey allStoriesF)
  let userF = UserFixtures {..}
      storyF = StoryFixtures {..}
      tagF = TagFixtures {..}
      taggingF = TaggingFixtures {..}
  return Fixtures {..}


getTables :: DB [Text]
getTables = do
  tables <- rawSql [st|
                      SELECT table_name
                      FROM information_schema.tables
                      WHERE table_schema = 'public'
                      AND table_type='BASE TABLE';
                   |] []
  return $ map unSingle tables

truncateAllTables :: DB ()
truncateAllTables = do
  tables <- getTables
  sqlBackend <- ask
  let escapedTables :: [Text]
      escapedTables =
        map (connEscapeName sqlBackend . DBName) tables
      query =
        [st|TRUNCATE TABLE #{intercalate ", " escapedTables} RESTART IDENTITY CASCADE|]
  case escapedTables of
    [] ->
      error "List of tables is empty, something has gone wrong!"
    _ -> rawExecute query []

wipeAndReinstallFixtures :: DB ()
wipeAndReinstallFixtures = do
  truncateAllTables
  void $ insertFixtures

wipeAndMigrateDatabase :: DB ()
wipeAndMigrateDatabase = do
  truncateAllTables
  runMigrations
  void $ insertFixtures

freshDatabase :: DB ()
freshDatabase = do
  runMigrations
  void $ insertFixtures
