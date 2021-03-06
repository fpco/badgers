module Handler.Home where

import Import hiding ((==.), on)

import qualified Data.Map as Map
import Data.Time.Clock
import Database.Esqueleto
import Data.Maybe (maybeToList)

import Helpers.Views
import Model

renderTag :: Tag -> Widget
renderTag Tag{..} =
  [whamlet|
<a .tag .tag_practices href="@{TagR tagTag}"
   title="Development and business practices">
  #{tagTag}
|]

renderStoryItem :: Story -> [Entity Tag] -> Widget
renderStoryItem Story{..} tags =
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
        $forall (Entity _ tag) <- tags
          ^{renderTag tag}
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

          <!-- <a href="/s/#{storyShortId}/lessons_learned_working_from_home"> -->
          <a href=@{CommentR storyShortId "this-is-a-slug"}>
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

type StoriesMap = Map StoryId (Entity Story, [Entity Tag])

getStoriesAndTags :: DB StoriesMap
getStoriesAndTags = do
  sat <- storiesAndTags
  return $ foldl' f Map.empty sat
  where
    storiesAndTags :: DB [(Entity Story, Maybe (Entity Tag))]
    storiesAndTags =
          select $
          from $
            \ (story `LeftOuterJoin` tagging `LeftOuterJoin` tag) -> do
              on (tag ?. TagId ==. tagging ?. TaggingTag)
              on (tagging ?. TaggingStory ==. just (story ^. StoryId))
              return (story, tag)

    f :: StoriesMap -> (Entity Story, Maybe (Entity Tag)) -> StoriesMap
    f storiesMap (story, maybeEntityTag) =
      let key = entityKey story
          newTag = maybeToList maybeEntityTag
          combineStoryTags _ (_, oldTags) = (story, newTag ++ oldTags)
      in Map.insertWith combineStoryTags key (story, newTag) storiesMap

getHomeR :: Handler Html
getHomeR = do
  databaseStories <- runDB $ getStoriesAndTags
  baseLayout Nothing $ do
    setTitle "Home"
    [whamlet|
<ol .stories .list>
  $forall (Entity _ story, tags) <- databaseStories
    ^{renderStoryItem story tags}
|]

getTagR :: Text -> Handler Html
getTagR tag = do
  baseLayout Nothing $ do
    setTitle "Tag"
    [whamlet|
<h1>#{tag}
|]
