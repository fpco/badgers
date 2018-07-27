module Handler.Comment where

import Import hiding ((==.), on)

import qualified Data.Map as Map
import Data.Time.Clock
import Data.Maybe (fromJust)
import Database.Esqueleto
import Data.Maybe (maybeToList)

import Handler.Comment.Query
import Handler.Comment.Views
import Helpers.Views
import Model

getCommentR :: Text -> Text -> Handler Html
getCommentR storyShortId _ = do
  (story, commentTrees) <- runDB $ getStoryAndCommentTree storyShortId
  let userId = storyUserId $ entityVal story
  user <- runDB $ fromJust <$> get userId
  baseLayout Nothing $ do
    setTitle "Home"
    [whamlet|
^{storyLiner}
$forall rootCommentTree <- commentTrees
  ^{renderCommentSubtree story (Entity userId user) rootCommentTree}
|]
