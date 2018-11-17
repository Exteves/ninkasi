{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Packing where

import Import
import Data.Time
import Handler.Funcs as F

optionsPackingR :: Handler ()
optionsPackingR = F.anyOriginIn [ F.OPTIONS, F.POST ]

-- POST --

postPackingR :: Handler Value
postPackingR = do
    packing    <-  requireJsonBody :: Handler Packing
    packingId  <-  runDB $ insert packing
    sendStatusJSON created201 $ object [ "resp" .= packingId ]


