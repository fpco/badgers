module Handler.Home where

import Import

import Data.Time.Clock

import Helpers.Views
import Model

-- Story { storyCreatedAt :: !UTCTime,
--         storyUserId :: !Key User,
--         storyUrl :: !Text,
--         storyTitle :: !Text,
--         storyDescription :: !Text,
--         storyShortId :: !Text,
--         storyIsExpired :: !Bool,
--         storyUpvotes :: !Int64,
--         storyDownvotes :: !Int64,
--         storyIsModerated :: !Bool,
--         storyHotness :: !Hotness,
--         storyMarkeddownDescription :: !Text,
--         storyStoryCache :: !Text,
--         storyCommentsCount :: !Int64,
--         storyMergedStoryId :: !Maybe (Key Story),
--         storyUnavailableAt :: !Maybe UTCTime,
--         storyTwitterId :: !Text,
--         storyUserIsAuthor :: !Bool}

-- defaultUser :: User
-- defaultUser =
--   let a = undefined
--   in User{..}

defaultStory :: Story
defaultStory =
  let storyCreatedAt = UTCTime (ModifiedJulianDay 10000) 0
      storyUserId = toSqlKey 0
      storyUrl = "http://www.google.com"
      storyTitle = "My Story Title"
      storyDescription = "My story description"
      storyShortId = "ABC123"
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
  in Story{..}

mockStoryValues :: [Story]
mockStoryValues = -- fmap (defaultUser,)
  [ defaultStory { storyTitle = "Lessons learned Working From Home" }
  , defaultStory { storyTitle = "Static Sites vs CMS" }
  , defaultStory { storyTitle = "First Impressions of the Rust Programming Language" }
  ]

renderStoryItem :: Story -> Widget
renderStoryItem Story{..} =
  [whamlet|
<li .story data-shortid="hujw8d" #story_hujw8d>
  <div .story_liner>
    <div .voters>
      <a .upvoter>
      <div .score>
        22
    <div .details>
      <span .link>
        <a href="https://sgoel.org/posts/lessons-learned-working-from-home/">
          #{storyTitle}
      <span .tags>
        <a .tag .tag_practices href="/t/practices" title="Development and business practices">
          practices
      <a .domain href="/search?order=newest&amp;q=domain:sgoel.org">
        sgoel.org
      <div .byline>
        <a href="/u/siddhantgoel">
          <img alt="siddhantgoel avatar" .avatar height="16" src="/avatars/siddhantgoel-16.png" srcset="/avatars/siddhantgoel-16.png 1x, /avatars/siddhantgoel-32.png 2x" width="16">
        authored by

        <a .user_is_author href="/u/siddhantgoel">
          siddhantgoel
        <span title="2018-06-08 03:28:44 -0500">
          13 hours ago
        | 
        <a .suggester href="/stories/hujw8d/suggest">
          suggest
        | 
        <a .flagger>
          flag
        | 
        <a .hider href="/stories/hujw8d/hide">
          hide
        | 
        <a .saver href="/stories/hujw8d/save">
          save
        |

        <a href="https://archive.is/https%3A%2F%2Fsgoel.org%2Fposts%2Flessons-learned-working-from-home%2F" rel="nofollow" target="_blank">
          cached
        <span .comments_label>
          |

          <a href="/s/hujw8d/lessons_learned_working_from_home">
            13
                            comments

  <div .mobile_comments style="display: none;">
    <a href="/s/hujw8d/lessons_learned_working_from_home">
      13
|]

getHomeR :: Handler Html
getHomeR =
  baseLayout Nothing $ do
    setTitle "Home"
    [whamlet|
<ol .stories .list>
  $forall story <- mockStoryValues
    ^{renderStoryItem story}
|]
