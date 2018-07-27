{-# LANGUAGE DeriveTraversable #-}

module Handler.Comment.Query where

import Import

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Time.Clock
import Database.Esqueleto hiding ((==.), selectFirst)
import qualified Database.Persist as P
import Data.Maybe (maybeToList)

import Helpers.Views
import Model

type StoryComments = (Entity Story, CommentTree)

type CommentMap =
  Map (Maybe CommentId) [Entity Comment]

data RoseTree a = RoseTree a [RoseTree a]
  deriving (Show, Eq, Ord, Foldable, Functor, Traversable)

type CommentTree = [RoseTree (Entity Comment)]

mkCommentMap :: [Entity Comment] -> CommentMap
mkCommentMap = foldl' f Map.empty
  where
    f commentMap commentE@(Entity _ comment) =
      Map.insertWith (++) (commentParentComment comment) [commentE] commentMap

mkCommentTree :: CommentMap -> [RoseTree (Entity Comment)]
mkCommentTree commentMap =
  mkCommentTree' commentMap <$> rootComments
  where
    rootComments = fromMaybe [] $ commentMap Map.!? Nothing

mkCommentTree' :: CommentMap -> Entity Comment -> RoseTree (Entity Comment)
mkCommentTree' commentMap rootComment@(Entity commentKey _) =
  RoseTree rootComment (mkCommentTree' commentMap <$> childComments)
  where
    childComments = fromMaybe [] $ commentMap Map.!? Just commentKey

getStoryAndCommentTree :: Text -> DB StoryComments
getStoryAndCommentTree shortCode = do
  story <- fromJust <$> P.selectFirst [StoryShortId ==. shortCode] [] :: DB (Entity Story)
  comments <- selectList [CommentStory ==. entityKey story ] [] :: DB [Entity Comment]
  let tree = mkCommentTree . mkCommentMap $ comments
  return (story, tree)
