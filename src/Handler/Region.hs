{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Region where

import Import
import Handler.Funcs as F

optionsRegionR :: Handler ()
optionsRegionR = F.anyOriginIn [ F.OPTIONS, F.POST ]

-- POST --

postRegionR :: Handler Value
postRegionR = do
    region      <-  requireJsonBody :: Handler Region
    regionId    <-  runDB $ insert region
    sendStatusJSON created201 $ object [ "resp" .= regionId ]


