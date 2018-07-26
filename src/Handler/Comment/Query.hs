module Handler.Comment.Query where

import Import hiding ((==.), on)

import qualified Data.Map as Map
import Data.Time.Clock
import Database.Esqueleto
import Data.Maybe (maybeToList)

import Helpers.Views
import Model

type StoryComments = Map StoryId (Entity Story, [Entity Comment])

type CommentTree =
  ([Entity Comment], Map CommentId [Entity Comment])

-- data RoseTree a = RoseTree a [RoseTree a]
-- [RoseTree (Entity Comment)]

getStoryAndComments :: Text -> DB StoryComments
getStoryAndComments shortCode = undefined
