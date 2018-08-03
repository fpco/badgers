{-# LANGUAGE DeriveTraversable #-}

module Handler.Comment.Query where

import Import

import qualified Data.Map as Map
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

data GetStoryCommentsException =
  CouldntFindStoryByShortId Text
  deriving (Eq, Show)

instance Exception GetStoryCommentsException

justOrThrowIO :: (Exception e, MonadIO m) => e -> m (Maybe a) -> m a
justOrThrowIO e ma = do
  aM <- ma
  case aM of
    Nothing -> throwIO e
    (Just a) -> return a

rightOrThrowIO :: (Exception e, MonadIO m) => m (Either e a) -> m a
rightOrThrowIO ma = do
  aM <- ma
  case aM of
    (Left e) -> throwIO e
    (Right a) -> return a

getStoryAndCommentTree :: Text -> DB StoryComments
getStoryAndCommentTree shortCode =
  rightOrThrowIO $ getStoryAndCommentTreeE shortCode

getStoryAndCommentTreeE :: Text -> DB (Either GetStoryCommentsException StoryComments)
getStoryAndCommentTreeE shortCode = do
  storyM <-
    P.selectFirst [StoryShortId ==. shortCode] []
  case storyM of
    Nothing -> return $ Left $ CouldntFindStoryByShortId shortCode
    (Just story) -> do
      comments <- selectList [CommentStory ==. entityKey story ] []
      let tree = mkCommentTree . mkCommentMap $ comments
      return $ Right (story, tree)
      where couldntFind =
              CouldntFindStoryByShortId shortCode
