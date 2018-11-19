{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Tasting where

import Import
import Handler.Funcs as F

-- OPTIONS --

optionsTastingR :: Handler ()
optionsTastingR = F.anyOriginIn [ F.OPTIONS, F.POST ]

optionsTastingByUserR :: Handler ()
optionsTastingByUserR = F.anyOriginIn [ F.OPTIONS, F.GET ]

-- POST --

postTastingR :: Handler Value
postTastingR = do
    anyOriginIn [ F.OPTIONS, F.POST ]
    tasting    <-  requireJsonBody :: Handler Tasting
    tastingId  <-  runDB $ insert tasting
    sendStatusJSON created201 $ object [ "resp" .= tastingId ]

getTastingByUserR :: Handler Value
getTastingByUserR = do
    anyOriginIn [ F.OPTIONS, F.GET ]
    token       <- getTokenHeader
    maybeUsr    <- runDB $ selectFirst [ UsrToken ==. token ] [ ]
    case maybeUsr of
        Just (Entity uid usr) -> do
            _           <- runDB $ get404 uid
            has         <- runDB $ selectList [ HasUsrId ==. uid ] [ ]
            tastingid   <- return $ fmap(\ls -> hasTastingId $ entityVal ls) has
            tasting     <- runDB $ selectList [TastingId <-. tastingid] [Asc TastingRating]
            sendStatusJSON ok200 $ object ["resp" .= tasting]
        _ -> sendStatusJSON forbidden403 $ object ["resp" .= ("acao proibida"::Text)]