{-# LANGUAGE RankNTypes #-}
module Yesod.Widget.GoogleAnalytics where

import Yesod
import Data.Text (Text)

googleAnalytics :: forall sub master. Text -- ^ Account code.
             -> Text -- ^ Domain name.
             -> GWidget sub master ()
googleAnalytics account domain =
    toWidget [julius|
    var _gaq = _gaq || [];
    _gaq.push(['_setAccount', '#{account}']);
    _gaq.push(['_setDomainName', '#{domain}']);
    _gaq.push(['_trackPageview']);

    (function() {
        var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
        ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
        var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
    })();
    |]

