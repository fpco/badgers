module Handler.Comment where

import Import hiding ((==.), on)

import qualified Data.Map as Map
import Data.Time.Clock
import Database.Esqueleto
import Data.Maybe (maybeToList)

import Handler.Comment.Query
import Handler.Comment.Views
import Helpers.Views
import Model

getCommentR :: Text -> Text -> Handler Html
getCommentR storyShortId _ = do
  baseLayout Nothing $ do
    setTitle "Home"
    [whamlet|
^{storyLiner}
^{renderCommentSubtree undefined undefined undefined undefined}
|]
