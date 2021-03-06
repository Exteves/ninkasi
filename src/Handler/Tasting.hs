{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Tasting where

import Import
import Handler.Funcs as F
import Database.Persist.Postgresql

-- OPTIONS --

optionsTastingR :: Handler ()
optionsTastingR = F.anyOriginIn [ F.OPTIONS, F.POST ]

optionsTastingByUserR :: Handler ()
optionsTastingByUserR = F.anyOriginIn [ F.OPTIONS, F.GET ]

optionsTastingByBeerIdR :: BeerId -> Handler ()
optionsTastingByBeerIdR _ = F.anyOriginIn [ F.OPTIONS, F.GET ]

optionsTastingByIdR :: TastingId -> Handler ()
optionsTastingByIdR _ = F.anyOriginIn [ F.OPTIONS, F.GET, F.DELETE ]

-- POST --

postTastingR :: Handler Value
postTastingR = do
    anyOriginIn [ F.OPTIONS, F.POST ]
    token       <- getTokenHeader
    maybeUsr    <- runDB $ selectFirst [UsrToken ==. token] []
    case maybeUsr of
        Just (Entity uid usr) -> do
            tasting <-  requireJsonBody :: Handler Tasting
            tid     <-  runDB $ insert tasting
            pid     <-  runDB $ insert (Has uid tid)
            sendStatusJSON created201 $ object ["id" .= fromSqlKey tid]
        _ -> sendStatusJSON forbidden403 $ object ["resp" .= ("acao proibida"::Text)]

getTastingR :: Handler Value
getTastingR = do
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
    
getTastingByBeerIdR :: BeerId -> Handler Value
getTastingByBeerIdR beerid = do
    anyOriginIn [ F.OPTIONS, F.GET ]
    tastingList     <- runDB $ selectList [ TastingBeer ==. beerid ] []
    tastingListM    <- return $ map entityKey tastingList
    sendStatusJSON ok200 $ object [ "resp" .= tastingListM ]

getTastingByIdR :: TastingId -> Handler Value
getTastingByIdR tastingId = do
    F.anyOriginIn [ F.OPTIONS, F.GET ]
    tasting   <-  runDB $ get404 tastingId
    sendStatusJSON ok200 $ object [ "resp" .= tasting ]

deleteTastingByIdR :: TastingId -> Handler Value
deleteTastingByIdR tastingId = do
    F.anyOriginIn [ F.OPTIONS, F.DELETE ]
    runDB $ delete tastingId
    sendStatusJSON ok200 $ object ["resp" .= ("ok"::Text)]