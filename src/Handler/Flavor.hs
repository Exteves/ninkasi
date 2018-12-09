{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Flavor where

import Import
import Handler.Funcs as F

optionsFlavorR :: Handler ()
optionsFlavorR = F.anyOriginIn [ F.OPTIONS, F.POST, F.GET ]

-- POST --

postFlavorR :: Handler Value
postFlavorR = do
    F.anyOriginIn [ F.OPTIONS, F.POST ]
    flavor    <-  requireJsonBody :: Handler Flavor
    flavorId  <-  runDB $ insert flavor
    sendStatusJSON created201 $ object [ "resp" .= flavorId ]

getFlavorR :: Handler Value
getFlavorR = do
    F.anyOriginIn [ F.OPTIONS, F.GET ]
    flavors   <-  runDB $ selectList [] [ Asc FlavorName ]
    sendStatusJSON ok200 $ object [ "resp" .= flavors ]

