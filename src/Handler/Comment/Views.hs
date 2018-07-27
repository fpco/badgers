module Handler.Comment.Views where

import Import hiding ((==.), on)

import qualified Data.Map as Map
import Data.Time.Clock
import Database.Esqueleto
import Data.Maybe (maybeToList)

import Helpers.Views
import Handler.Comment.Query
import Model

storyLiner :: Widget
storyLiner = [whamlet|
<div .story_liner>
  <div .voters>
    <a .upvoter>
    <div .score>
      19
  <div .details>
    <span .link>
      <a href="https://www.sicpers.info/2018/07/is-freedom-zero-such-a-hot-idea/">
        Is Freedom Zero such a hot idea?
    <span .tags>
      <a .tag .tag_law href="/t/law" title="Law, patents, and licensing">
        law
    <a .domain href="/search?order=newest&amp;q=domain:sicpers.info">
      sicpers.info
    <div .byline>
      <a href="/u/leeg">
        <img alt="leeg avatar" .avatar height="16" src="/avatars/leeg-16.png" srcset="/avatars/leeg-16.png 1x, /avatars/leeg-32.png 2x" width="16">
      authored by

      <a .user_is_author href="/u/leeg">
        leeg
      <span title="2018-07-24 13:17:39 -0500">
        2 days ago
      |
      <a .suggester href="/stories/gpzhu8/suggest">
        suggest
      |
      <a .flagger>
        flag
      |
      <a .hider href="/stories/gpzhu8/hide">
        hide
      (hidden by 6 users)
                  |
      <a .saver href="/stories/gpzhu8/save">
        save
      |

      <a href="https://archive.is/https%3A%2F%2Fwww.sicpers.info%2F2018%2F07%2Fis-freedom-zero-such-a-hot-idea%2F" rel="nofollow" target="_blank">
        cached
      <span .comments_label>
        |

        <a href="/s/gpzhu8/is_freedom_zero_such_hot_idea">
          27
                          comments

      | +23, -1 off-topic, -3 spam
|]

postTopLevelComment :: Widget
postTopLevelComment = [whamlet|
<ol .comments .comments1>
  <li .comments_subtree>
    <div .comment .comment_form_container data-shortid="">
      <form accept-charset="UTF-8" action="/comments" .new_comment #edit_comment_ method="post">
        <input name="utf8" type="hidden" value="✓">
        <input name="authenticity_token" type="hidden" value="6ToyBkRFKWorWGgA4xzlGhiNRn7HzIBYTK3s++Thj/El/oZpt15M/zRXKuSS2DZZF5FDv81NszWokwiSC4B+aQ==">
        <input #story_id name="story_id" type="hidden" value="gpzhu8">
        <div style="width: 100%;">
          <textarea data-autosize-on="true" #comment name="comment" placeholder="" rows="5" style="overflow: hidden visible; overflow-wrap: break-word; resize: none; height: 102.997px;">
          <p>
          <div .markdown_help_toggler>
            <div .markdown_help_label>
              Markdown formatting available

            <div .markdown_help_label .markdown_help_label_mobile style="display: none;">
              [M↓]

            <input .comment-post data-disable-with="Post" name="commit" type="submit" value="Post">
            <button .comment-preview name="button" type="button">
              Preview
            <div style="clear: both;">
            <div .markdown_help style="display: none; padding-top: 0.5em;">
              <table>
                <tbody>
                  <tr>
                    <td width="125">
                      <em>
                        emphasized text
                    <td>
                      surround text with
                      <tt>
                        *asterisks*
                  <tr>
                    <td>
                      <strong>
                        strong text
                    <td>
                      surround text with
                      <tt>
                        **two asterisks**
                  <tr>
                    <td>
                      <strike>
                        struck-through
                    <td>
                      surround text with
                      <tt>
                        ~~two tilde characters~~
                  <tr>
                    <td>
                      <tt>
                        fixed width
                    <td>
                      surround text with
                      <tt>
                        `backticks`
                  <tr>
                    <td>
                      <a href="http://example.com/" style="color: inherit;">
                        linked text
                    <td>
                      <tt>
                        [linked text](http://example.com/)
                      or just a bare URL
                            to create without a title
                  <tr>
                    <td>
                      <blockquote>
                        quoted text
                    <td>
                      prefix text with
                      <tt>
                        >
                  <tr>
                    <td>
                      <pre style="margin: 0px;">
                        pre
                          text
                    <td>
                      prefix text with at least
                      <tt>
                        4 spaces
        <p>
  <script>
    autosize($("#comment")[0]);
|]

renderCommentSubtree :: Entity Story
                     -> Entity User
                     -> RoseTree (Entity Comment)
                     -> Widget
renderCommentSubtree story user (RoseTree comment childTrees) = [whamlet|
<li .comments_subtree>
  <input .comment_folder_button #comment_folder_trz9yd type="checkbox">
  ^{renderComment story user comment}
  <ol .comments>
    $forall childTree <- childTrees
      ^{renderCommentSubtree story user childTree}
|]

renderComment :: Entity Story -> Entity User -> Entity Comment -> Widget
renderComment (Entity _ Story{..}) (Entity _ User{..}) (Entity commentK Comment{..}) =
  [whamlet|
<div .comment
     data-shortid="#{commentShortId}"
     id="#{commentShortId}">
  <label .comment_folder
         for="comment_folder_#{commentShortId}">
  <div .voters>
    <a .upvoter>
    <div .score>
      #{commentUpvotes - commentDownvotes}
    <a .downvoter>
  <div .comment_parent_tree_line
       .can_downvote .score_shown>
  <div .details>
    <div .byline>
      <a name="c_#{commentShortId}">
      <label .comment_folder
             .comment_folder_inline
             for="comment_folder_#{commentShortId}">
      <a href="/u/#{userUsername}">
        <img alt="#{userUsername} avatar" .avatar height="16" src="/avatars/#{userUsername}-16.png" srcset="/avatars/#{userUsername}-16.png 1x, /avatars/#{userUsername}-32.png 2x" width="16">
      <a href="/u/#{userUsername}">
        #{userUsername}
      <span title="#{tshow commentCreatedAt}">
        #{tshow commentCreatedAt}
      |

      <a href="https://lobste.rs/s/#{storyShortId}/is_freedom_zero_such_hot_idea#c_#{commentShortId}">
        link
      |

      <a .comment_replier tabindex="0" unselectable="on">
        reply
      <span .reason>
    <div .comment_text>
      <pre>
        #{commentComment}
|]
