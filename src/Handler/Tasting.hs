{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Tasting where

import Import
import Handler.Funcs as F

optionsTastingR :: Handler ()
optionsTastingR = F.anyOriginIn [ F.OPTIONS, F.POST ]

-- POST --

postTastingR :: Handler Value
postTastingR = do
    tasting    <-  requireJsonBody :: Handler Tasting
    tastingId  <-  runDB $ insert tasting
    sendStatusJSON created201 $ object [ "resp" .= tastingId ]