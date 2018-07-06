module Handler.Home where

import Import hiding ((==.), on)

import qualified Data.Map as Map
import Data.Time.Clock
import Database.Esqueleto

import Helpers.Views
import Model

-- [Tag] -> Int64
-- [Vote]
-- [VoteStory]
-- [VoteComment]

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
            13 comments

  <div .mobile_comments style="display: none;">
    <a href="/s/hujw8d/lessons_learned_working_from_home">
      13
|]

getTagsForStory :: StoryId -> DB [Entity Tag]
getTagsForStory storyId =
  select $
  from $
    \ (tagging `InnerJoin` tag) -> do
      on (tag ^. TagId ==. tagging ^. TaggingTag)
      where_ (tagging ^. TaggingStory ==. val storyId)
      return tag

getStoriesThatHaveTags :: DB [(Entity Story, Entity Tag)]
getStoriesThatHaveTags =
  select $
  from $
    \ (story `InnerJoin` tagging `InnerJoin` tag) -> do
      on (tag ^. TagId ==. tagging ^. TaggingTag)
      on (tagging ^. TaggingStory ==. story ^. StoryId)
      return (story, tag)

-- [(Entity Story, [Entity Tag])]

-- data StoryAndTags = StoryAndTags {
--     satStory :: Entity Story
--   , satTags :: [Entity Tag]
--   }
--   deriving Show

-- data StoryAndTags' f = StoryAndTags' {
--     satStory :: f Story
--   , satTags :: [f Tag]
--   }
--   deriving Show

-- type StoryAndTags = StoryAndTags' Entity

-- type StoryAndTagsNonDB = StoryAndTags' Identity

-- getStoriesAndTags :: DB [(Entity Story, Entity Tag)]
-- getStoriesAndTags :: DB [(Entity Story, Maybe (Entity Tag))]

-- getStoriesAndTags :: DB [(Entity Story, [Entity Tag])]
type StoriesMap = Map StoryId (Entity Story, [Entity Tag])

getStoriesAndTags :: DB StoriesMap
getStoriesAndTags = do
  sat <- storiesAndTags
  return $ foldl' f Map.empty sat
  where storiesAndTags =
          select $
          from $
            \ (story `LeftOuterJoin` tagging `LeftOuterJoin` tag) -> do
              on (tag ?. TagId ==. tagging ?. TaggingTag)
              on (tagging ?. TaggingStory ==. just (story ^. StoryId))
              return (story, tag)

        maybeToList :: Maybe a -> [a]
        maybeToList Nothing = []
        maybeToList (Just a) = [a]

        f :: StoriesMap -> (Entity Story, Maybe (Entity Tag)) -> StoriesMap
        f storiesMap (story, maybeEntityTag) =
          let key = entityKey story
              newTag = maybeToList maybeEntityTag
              adjuster (_, xs) =
                (story, newTag ++ xs)
          in case Map.lookup key storiesMap of
               Nothing ->
                 Map.insert key (story, newTag) storiesMap
               (Just _) ->
                 Map.adjust adjuster key storiesMap

getHomeR :: Handler Html
getHomeR = do
  databaseStories <- runDB $ getStoriesAndTags
  baseLayout Nothing $ do
    setTitle "Home"
    [whamlet|
<ol .stories .list>
  $forall (Entity _ story, _) <- databaseStories
    ^{renderStoryItem story}
|]
