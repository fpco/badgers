module Helpers.Views where

import Import.NoFoundation

baseLayout :: (Yesod site)
           => Maybe (Entity User)
           -> WidgetT site IO ()
           -> HandlerT site IO Html
baseLayout _ content =
  defaultLayout $ do
    [whamlet|
^{content}
|]
