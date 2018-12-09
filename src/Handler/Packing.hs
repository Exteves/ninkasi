{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Packing where

import Import
import Handler.Funcs as F

optionsPackingR :: Handler ()
optionsPackingR = F.anyOriginIn [ F.OPTIONS, F.POST, F.GET ]

-- POST --

postPackingR :: Handler Value
postPackingR = do
    F.anyOriginIn [ F.OPTIONS, F.POST ]
    packing    <-  requireJsonBody :: Handler Packing
    packingId  <-  runDB $ insert packing
    sendStatusJSON created201 $ object [ "resp" .= packingId ]

getPackingR :: Handler Value
getPackingR = do
    F.anyOriginIn [ F.OPTIONS, F.GET ]
    packtypes   <-  runDB $ selectList [] [ Asc PackingName ]
    sendStatusJSON ok200 $ object [ "resp" .= packtypes ]
