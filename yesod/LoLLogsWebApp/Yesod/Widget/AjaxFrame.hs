module Yesod.Widget.AjaxFrame 
    ( ajaxFrame
    , FrameOptions(..)
    ) where

import Prelude
import Yesod
import Data.Text (Text)

data FrameOptions = FrameOptions
    { linkSelector  :: Text
    , inputSelector :: Text
    , formSelector  :: Text
    }

defaultFrameOptions = FrameOptions
    { linkSelector  = "a"
    , inputSelector = "input, select"
    , formSelector  = "form"
    }

ajaxFrame :: GWidget sub master () -> GWidget sub master ()
ajaxFrame content = do
    frameId <- lift newIdent
    [julius|
        $(function () {
            alert("Foo");
        });
    |]
    [whamlet|
<div##{frameId}>
    ^{content}
|]
