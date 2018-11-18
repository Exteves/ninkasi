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
            runDB $ update uid [usrToken =. (usrToken newHashUser)]
            sendStatusJSON ok200 (object ["resp" .= (usrToken newHashUser) ])
        Nothing -> 
            sendStatusJSON status404 (object ["resp" .= ("Usuário não cadastrado"::Text)] )