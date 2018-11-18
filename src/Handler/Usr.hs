{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Usr where

import Import
import Handler.Funcs as F
import Data.Text as T
import Yesod.Auth.HashDB (setPassword)

-- OPTIONS --

optionsLoginnR :: Handler ()
optionsLoginnR = F.anyOriginIn [ F.OPTIONS, F.POST ]

optionsRegisterR :: Handler ()
optionsRegisterR = F.anyOriginIn [ F.OPTIONS, F.POST ]

optionsLogouttR :: Handler ()
optionsLogouttR = anyOriginIn [ F.OPTIONS, F.POST ]

-- POST --

postLoginnR :: Handler Value    
postLoginnR = do
    anyOriginIn [ F.OPTIONS, F.POST ]
    (email, passwd) <- requireJsonBody :: Handler (Text,Text)
    maybeUsr        <- runDB $ getBy $ UsrLogin email passwd
    case maybeUsr of
        Just (Entity uid usr) -> do
            newHashUser <- setPassword (usrEmail usr) usr
            runDB $ update uid [UsrToken =. (usrToken newHashUser)]
            sendStatusJSON ok200 (object ["resp" .= (usrToken newHashUser) ])
        Nothing -> 
            sendStatusJSON status404 (object ["resp" .= ("Usuario nao cadastrado"::Text)] )

postRegisterR :: Handler Value
postRegisterR = do
    anyOriginIn [ F.OPTIONS, F.POST ]
    usu <- requireJsonBody :: Handler Usr
    hashUser <- setPassword (usrEmail usu) usu
    usrId <- runDB $ insert hashUser
    sendStatusJSON created201 (object ["resp" .= (usrToken hashUser)])

postLogouttR :: Handler Value
postLogouttR = do 
    token <- getTokenHeader
    anyOriginIn [ F.OPTIONS, F.POST ]
    maybeUser <- runDB $ selectFirst [UsrToken ==. token] []
    case maybeUser of
        Just (Entity uid usr) -> do
            newHashUser <- setPassword (usrEmail usr) usr
            runDB $ update uid [UsrToken =. (usrToken newHashUser)]
            sendStatusJSON ok200 (object ["resp" .= ("usuario deslogado"::Text)])
        _ -> 
            sendStatusJSON status404 (object ["resp" .= ("Usuário não cadastrado"::Text)] )