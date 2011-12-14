module Yesod.Widget.AjaxFrame 
    ( ajaxFrame
    , FrameOptions(..)
    , defaultFrameOptions
    ) where

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

--ajaxFrame :: GWidget sub master () -> GWidget sub master ()
ajaxFrame options content = do
    frameId <- lift newIdent
    toWidget [julius|
        $(function () {
            var $container = $('##{frameId}');
            $container.delegate('#{linkSelector options}', 'click', function (e) {
                $container.addClass('loading');

                e.preventDefault();

                var href = $(this).attr('href');

                ajaxUpdate(href);

            }).end();

            function ajaxUpdate(href) {
                $.ajax({
                    url: href,
                    success: function (data, textStatus, jqXHR) {
                        $container.html( $("##{frameId}", data).html() );
                        $container.removeClass('loading');
                    },
                    error: function (jqXHR, textStatus, errorThrown) {
                        $container.removeClass('loading');
                        alert("an error occured.");
                    }
                });
            }
        });
    |]
    [whamlet|
<div##{frameId}>
    ^{content}
|]