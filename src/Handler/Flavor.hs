{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Flavor where

import Import
import Handler.Funcs as F

optionsFlavorR :: Handler ()
optionsFlavorR = F.anyOriginIn [ F.OPTIONS, F.POST ]

-- POST --

postFlavorR :: Handler Value
postFlavorR = do
    flavor    <-  requireJsonBody :: Handler Flavor
    flavorId  <-  runDB $ insert flavor
    sendStatusJSON created201 $ object [ "resp" .= flavorId ]


