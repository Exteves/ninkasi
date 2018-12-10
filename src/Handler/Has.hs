{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Has where

import Import
import Handler.Funcs as F
import qualified Data.Maybe as M

--OPTIONS--

optionsHasR :: TastingId -> Handler ()
optionsHasR _ = F.anyOriginIn [ F.OPTIONS, F.POST ]

-- POST --

postHasR :: TastingId -> Handler Value
postHasR tastingid = do
    anyOriginIn [ F.OPTIONS, F.POST ]
    token       <-  getTokenHeader
    maybeUsr    <-  runDB $ selectFirst [ UsrToken ==. token ] [ ]
    case maybeUsr of
        Just (Entity uid usr) -> do
            has <- runDB $ selectFirst [ HasTastingId ==. tastingid, HasUsrId ==. uid ] [ ]
            pss <- runDB $ replace (entityKey (M.fromJust has)) (Has uid tastingid)
            sendStatusJSON ok200 $ object ["resp" .= ("ok"::Text)]
        _ -> sendStatusJSON forbidden403 $ object ["resp" .= ("acao proibida"::Text) ]

